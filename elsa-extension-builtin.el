;;; elsa-extension-builtin.el --- Basic extensions for builtin packages -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created:  9th March 2023

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'f)

(require 'elsa-log)
(require 'elsa-reader)
(require 'elsa-analyser)

(require 'elsa-extension-subr)
(require 'elsa-extension-eieio)
(require 'elsa-extension-cl)

(require 'elsa-typed-builtin)

(require 'elsa-typed-syntax)

(defun elsa--autoload-types (global-state dep)
  (when (require (intern (concat "elsa-typed-"
                                 (replace-regexp-in-string ".el\\'" "" dep)))
                 nil t)
    (elsa-log "%sAutoloading types for %s"
              (make-string (elsa-global-state-prefix-length global-state 3) ? )
              dep)))

(defun elsa--autoload-extension (global-state dep)
  (when (require (intern (concat "elsa-extension-"
                                 (replace-regexp-in-string ".el\\'" "" dep)))
                 nil t)
    (elsa-log "%sAutoloading extension for %s"
              (make-string (elsa-global-state-prefix-length global-state 3) ? )
              dep)))

(defun elsa--analyse:require (form scope state)
  (let* ((feature (elsa-nth 1 form))
         (feature-name (symbol-name (elsa-get-name feature))))
    (elsa--autoload-types (oref state global-state) feature-name)
    (elsa--autoload-extension (oref state global-state) feature-name)
    (when (elsa--quoted-symbol-p feature)
      (push (elsa-get-name (elsa-cadr feature)) (oref state requires)))))

(defun elsa--analyse:provide (form scope state)
  (let ((feature (elsa-nth 1 form)))
    (when (elsa--quoted-symbol-p feature)
      (push (elsa-get-name (elsa-cadr feature)) (oref state provide)))))

(declare-function elsa-process-file "elsa" (file))

;; * boolean functions
(defun elsa--analyse:not (form scope state)
  (elsa--analyse-function-call form scope state)
  (let* ((args (cdr (oref form sequence)))
         (arg-type (oref (car args) type))
         (nullable (elsa-type-is-nil arg-type)))
    (cond
     ((trinary-true-p nullable) ;; definitely is nil
      (oset form type (elsa-type-t)))
     ((trinary-false-p nullable) ;; definitely is non-nil
      (oset form type (elsa-type-nil)))
     (t (oset form type (elsa-make-type bool))))))

(defun elsa--analyse:null (form scope state)
  (elsa--analyse:not form scope state))

(defun elsa--analyse--eq (eq-form symbol-form constant-form)
  "Setup narrowing for the EQ-FORM based on the SYMBOL-FORM variable.

CONSTANT-FORM is the value to which the variable is narrowed."
  (let ((name (elsa-get-name symbol-form))
        (type))
    (setq type
          ;; TODO: can't we check here that it is `const' type?
          (cond
           ((elsa-form-keyword-p constant-form) (elsa-make-type keyword))
           ((elsa--quoted-symbol-p constant-form) (elsa-make-type symbol))
           ((and (elsa-form-symbol-p constant-form)
                 (eq (elsa-get-name constant-form) t))
            (elsa-make-type t))
           ((and (elsa-form-symbol-p constant-form)
                 (eq (elsa-get-name constant-form) nil))
            (elsa-make-type nil))
           ((elsa-form-integer-p constant-form) (elsa-make-type int))
           ((elsa-form-float-p constant-form) (elsa-make-type float))))
    (when type
      (let ((narrow-type
             (if (or (elsa-type-t-p type)
                     (elsa-type-nil-p type))
                 type
               (elsa-const-type :type type
                                :value (cond
                                        ((elsa--quoted-symbol-name constant-form))
                                        ((elsa-get-name constant-form))
                                        ((oref constant-form value)))))))
        (oset eq-form narrow-types
              (list
               (elsa-variable :name name :type narrow-type)))))))

(defun elsa--analyse:eq (form scope state)
  (elsa--analyse-function-call form scope state)
  (let* ((args (elsa-cdr form))
         (first (car args))
         ;; This can be missing if `eq' was found inside a `pcase'
         ;; pattern.
         (second (cadr args)))
    (when (and first second)
      ;; If one or the other argument is a symbol, we will try to
      ;; narrow its type after the `eq' check based on the other
      ;; argument's type.
      (cond
       ((and (elsa-form-symbol-p first)
             (elsa-scope-get-var scope first))
        (elsa--analyse--eq form first second))
       ((and (elsa-form-symbol-p second)
             (elsa-scope-get-var scope second))
        (elsa--analyse--eq form second first)))
      ;; Here we compute the type of the eq form itself.  By default, it
      ;; has a mixed type, so here we only make the type information
      ;; more precise.
      (cond
       ((elsa-type-equivalent-p
         (elsa-type-empty)
         (elsa-type-intersect first second))
        (oset form type (elsa-type-nil)))
       ((and (elsa-const-type-p (elsa-get-type first))
             (elsa-const-type-p (elsa-get-type second))
             (elsa-type-equivalent-p first second))
        (oset form type (elsa-type-t)))))))

;; * list functions
(defun elsa--analyse:car (form scope state)
  (elsa--analyse-function-call form scope state)
  (-when-let* ((arg (cadr (oref form sequence)))
               (arg-type (oref arg type)))
    (cond
     ((elsa-type-list-p arg-type)
      (oset form type (elsa-type-make-nullable (oref arg-type item-type))))
     ((elsa-type-cons-p arg-type)
      (oset form type (oref arg-type car-type))))))

(defun elsa--analyse:cons (form scope state)
  (elsa--analyse-function-call form scope state)
  (-when-let* ((car-type (elsa-get-type (elsa-nth 1 form)))
               (cdr-type (elsa-get-type (elsa-nth 2 form))))
    (oset form type (elsa-type-cons :car-type car-type :cdr-type cdr-type))))

(defun elsa--analyse:elt (form scope state)
  (elsa--analyse-function-call form scope state)
  (-when-let* ((arg (cadr (oref form sequence)))
               (arg-type (oref arg type)))
    (when (elsa-instance-of arg-type (elsa-make-type sequence))
      (when-let* ((item-type (elsa-type-get-item-type arg-type))
                  ;; with lists it returns nil when overflowing, otherwise
                  ;; throws an error
                  (item-type (if (elsa-type-list-p arg-type)
                                 (elsa-type-make-nullable item-type)
                               item-type)))
        (oset form type item-type)))))

;; * control flow
(defun elsa--analyse:when (form scope state)
  (let ((condition (elsa-nth 1 form))
        (body (elsa-nthcdr 2 form))
        (return-type (elsa-type-empty)))
    (elsa--analyse-form condition scope state)
    (elsa-with-reachability state (elsa-type-is-non-nil condition)
      (elsa-save-scope scope
        (elsa-scope-narrow-var scope (oref condition narrow-types))
        (elsa--analyse-body body scope state)))
    (when body
      (setq return-type (oref (-last-item body) type)))
    (when (elsa-type-accept condition (elsa-type-nil))
      (setq return-type (elsa-type-make-nullable return-type))
      (when (elsa-type-accept (elsa-type-nil) condition)
        (setq return-type (elsa-type-nil))))
    (oset form type return-type)))

(defun elsa--analyse:unless (form scope state)
  (let ((condition (elsa-nth 1 form))
        (body (elsa-nthcdr 2 form))
        (return-type (elsa-type-nil)))
    (elsa--analyse-form condition scope state)
    (elsa-with-reachability state (elsa-type-is-nil condition)
      (elsa-save-scope scope
        (elsa-scope-narrow-var scope (oref condition narrow-types)
                               'elsa-variable-diff)
        (elsa--analyse-body body scope state)))
    (if (not (elsa-type-accept condition (elsa-type-nil)))
        (elsa-type-nil)
      (when body
        (setq return-type (oref (-last-item body) type)))
      (unless (elsa-type-equivalent-p (elsa-type-nil) condition)
        (setq return-type (elsa-type-make-nullable return-type))))
    (oset form type return-type)))

;; TODO: verify that the "rest" :args of the defgroup are valid
(defun elsa--analyse:defgroup (form scope state))

;; TODO: reachability
;; This is not always correct but at least we try to bind variables in
;; case the place is a simple symbol.  The logic is handled in
;; `elsa--analyse-variable-from-binding'
(defun elsa--analyse:when-let (form scope state)
  (let ((bindings (elsa-form-sequence (elsa-nth 1 form)))
        (body (cddr (oref form sequence)))
        (vars))
    (--each bindings
      (-when-let (var (elsa--analyse-variable-from-binding it scope state))
        (elsa-type-make-non-nullable var)
        (elsa-scope-add-var scope var)
        (push var vars)))
    (elsa--analyse-body body scope state)
    (--each vars (elsa-scope-remove-var scope it))
    (oset form type (elsa-get-type (-last-item body)))))

(defun elsa--analyse:list (form scope state)
  (let ((items (elsa-cdr form)))
    (elsa--analyse-body items scope state)
    ;; we need to promote constant types, because the chances this
    ;; tuple is of constant type is slim-to-none
    (let* ((types (-map #'elsa-get-type items))
           (types (--map
                   (if (elsa-const-type-p it)
                       (oref it type)
                     it)
                   types)))
      (oset form type (elsa-type-tuple :types types)))))

(defun elsa--analyse:funcall (form scope state)
  (let ((head (elsa-cadr form))
        (args (elsa-nthcdr 2 form)))
    ;; In case head is a variable, this should set its type to the
    ;; callable type it represents indirectly.
    (when head
      (elsa--analyse-form head scope state)
      (elsa--analyse-function-like-invocation
       form t head args scope state))))

(defun elsa--analyse:error (form scope state)
  "Analyse `error' form.

Since error stops the flow of the program, the form's type is
empty, because it has no value."
  (oset form type (elsa-type-empty)))

(defun elsa--analyse:eval-when-compile (form scope state)
  (elsa--analyse:progn form scope state))

(defun elsa--analyse:eval-and-compile (form scope state)
  (elsa--analyse:progn form scope state))

(defun elsa--analyse:with-suppressed-warnings (form scope state)
  (let ((body (elsa-nthcdr 2 form)))
    (elsa--analyse-body body scope state)))

(provide 'elsa-extension-builtin)
;;; elsa-extension-builtin.el ends here
