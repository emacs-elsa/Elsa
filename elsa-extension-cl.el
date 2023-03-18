;;; elsa-extension-cl.el --- Elsa extension for common lisp -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created:  6th March 2023
;; Package-requires: ((dash "2.17.0"))
;; Keywords:

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

(require 'map)

(require 'dash)

(require 'elsa-analyser)
(require 'elsa-type-helpers)
(require 'elsa-extension-eieio)

(require 'elsa-typed-cl)

(defun elsa--cl-analyse-specifiers (args-raw)
  "Analyse the cl-style specifiers to extract type information.

ARGS-RAW is the raw forms as present in the `cl-defmethod' or
`cl-defgeneric' arglist."
  (elsa-form-foreach args-raw
    (lambda (arg)
      (when (elsa-form-list-p arg)
        (-when-let* ((arg-form (elsa-car arg))
                     (type-annotation (elsa-cadr arg))
                     (type (elsa--cl-type-to-elsa-type
                            (elsa-form-to-lisp type-annotation))))
          (oset arg-form type type))))))

(defun elsa--analyse:cl-defmethod (form scope state)
  "cl-defmethod is like defun, but there can be extra
\"qualifiers\" after the method name.  These are a keywords or
keyword-sexp pairs."
  (let* ((name (elsa-get-name (elsa-cadr form)))
         (function-def (elsa-state-get-defun state name))
         (args-and-body (-drop-while (-not #'elsa-form-list-p)
                                     (elsa-nthcdr 2 form)))
         (args-raw (elsa-car args-and-body))
         (args (elsa-form-map args-raw
                 (lambda (arg)
                   (if (elsa-form-list-p arg)
                       (elsa-car arg)
                     arg))))
         (body (elsa-cdr args-and-body)))
    ;; FIXME: implement the datastructure to enable this check
    ;; (unless function-def
    ;;   (elsa-state-add-message state
    ;;     (elsa-make-warning (elsa-car form)
    ;;       "Missing `cl-defgeneric' for method %s"
    ;;       :code "missing-defgeneric"
    ;;       name)))
    ;; Here we attach "cl" type information to the arg forms which can
    ;; be used later during the defun analysis.
    (elsa--cl-analyse-specifiers args-raw)
    (when function-def
      (cond
       ;; clone always returns the same type as passed in
       ((eq name 'clone)
        (elsa-state-add-defun state
          (elsa-defun :name 'cl-call-next-method
                      :type (elsa-function-type
                             :args nil
                             :return (oref (elsa-car args-raw) type))
                      :arglist nil
                      :file (oref function-def file))))
       (t (elsa-state-add-defun state
            (elsa-defun :name 'cl-call-next-method
                        :type (elsa-function-type
                               :args nil
                               :return (elsa-type-get-return (oref function-def type)))
                        :arglist nil
                        :file (oref function-def file))))))
    (elsa--analyse-defun-like-form name args body form scope state)
    (when function-def
      (elsa-state-remove-defun state 'cl-call-next-method))))

;; FIXME: store the information that the defun is defgeneric so we can
;; compare signatures against the "abstract" function type if
;; specified.
(defun elsa--analyse:cl-defgeneric (form scope state)
  (let* ((name (elsa-get-name (elsa-cadr form)))
         (args-and-body (-drop-while (-not #'elsa-form-list-p)
                                     (elsa-nthcdr 2 form)))
         (args-raw (elsa-car args-and-body))
         (args (elsa-form-map args-raw
                 (lambda (arg)
                   (if (elsa-form-list-p arg)
                       (elsa-car arg)
                     arg))))
         (body (elsa-cdr args-and-body))
         (annotation (oref form annotation)))
    ;; check if the cl-defgeneric body is empty
    (if (or (eq (length (elsa-form-sequence form)) 3)
            (and (eq (length (elsa-form-sequence form)) 4)
                 (elsa-form-string-p (elsa-nth 3 form))))
        (let ((type (or (elsa--make-function-type-from-annotation
                         annotation)
                        (elsa-function-type
                         :args (elsa--get-default-function-types args)
                         :return (elsa-type-unbound)))))

          ;; If a definition already exists, this is an error because
          ;; we are redefining existing generic.
          (when-let ((def (elsa-state-get-defun state name)))
            (elsa-state-add-message state
              (elsa-make-error (elsa-car form)
                "Redefinition of generic function %s"
                :code "cl-defgeneric-redefinition"
                name)))

          ;; Defgeneric without a body does not define any callable
          ;; method, but we still need to register the function for
          ;; things like "go to definition" to work.  Also we need to
          ;; be able to compare defmethod signatures to be compatible
          ;; to the defgeneric.  We will set the return type to
          ;; unbound to mark this function as uncallable.
          (elsa-state-add-defun state
            (elsa-defun :name name
                        :defun-type 'cl-defgeneric
                        :defgeneric-type type
                        :type type
                        :arglist (elsa-form-to-lisp args))))
      (elsa--cl-analyse-specifiers args-raw)
      (elsa--analyse-defun-like-form name args body form scope state))))

(defun elsa--analyse:cl-defstruct (form scope state)
  (let* ((name (elsa-get-name (elsa-cadr form)))
         (params (when (elsa-form-list-p (elsa-cadr form))
                   (elsa-cdr (elsa-cadr form))))
         (slots (elsa-nthcdr 2 form)))

    ;; First "slot" could be a docstring, we need to skip it.
    (when (elsa-form-string-p (car slots))
      (pop slots))

    (elsa-state-add-defstruct state
      (elsa-defstruct
       :name name
       :slots (elsa-eieio--create-slots
               (mapcar
                (lambda (slot)
                  (cond
                   ((elsa-form-symbol-p slot)
                    (list (elsa-get-name slot) :type (elsa-type-mixed)))
                   ((elsa-form-list-p slot)
                    (let* ((slot-name (elsa-get-name slot))
                           (default (elsa-cadr slot))
                           (type (map-elt (elsa-nthcdr 2 slot) :type))
                           (read-only (map-elt (elsa-nthcdr 2 slot) :read-only))
                           (elsa-type (or (elsa--cl-type-to-elsa-type
                                           (elsa-form-to-lisp type))
                                          (elsa-type-mixed)))
                           (elsa-type (if read-only
                                          (elsa-readonly-type :type elsa-type)
                                        elsa-type)))
                      (when (elsa-form-keyword-p default)
                        (elsa-state-add-message state
                          (elsa-make-warning default
                            "Second argument of a slot definition list should be the default value, not an option %s"
                            (elsa-tostring default))))

                      (elsa-state-add-defun state
                        (elsa-defun
                         :name (intern (concat (symbol-name name) "-" (symbol-name slot-name)))
                         :type (elsa-function-type
                                :args (list (elsa--make-type `(struct ,name)))
                                :return elsa-type)
                         :arglist (list '&rest 'args)))

                      (list slot-name :type elsa-type)))))
                slots))
       :parents (list (list name))))

    (elsa-state-add-defun state
      (elsa-defun
       :name (intern (concat (symbol-name name) "-p"))
       :type (elsa--make-type `(function (mixed) (is (struct ,name))))
       :arglist (list 'x)))

    (let ((has-default-constructor t))
      (--each params
        (when (elsa-form-list-p it)
          (when-let ((ctr-def (map-elt it :constructor)))
            (if (elsa-get-name ctr-def)
                (elsa-state-add-defun state
                  (elsa-defun
                   :name (elsa-get-name ctr-def)
                   :type (elsa--make-type `(function (&rest mixed) (struct ,name)))
                   :arglist (list '&rest 'args)))
              (setq has-default-constructor nil)))))

      (when has-default-constructor
        (elsa-state-add-defun state
          (elsa-defun
           :name (intern (concat "make-" (symbol-name name)))
           :type (elsa--make-type `(function (&rest mixed) (struct ,name)))
           :arglist (list '&rest 'args)))))))

(defun elsa--analyse:cl-eval-when (form scope state)
  (let ((body (elsa-nthcdr 2 form)))
    (elsa--analyse-body body scope state)))

(provide 'elsa-extension-cl)
;;; elsa-extension-cl.el ends here
