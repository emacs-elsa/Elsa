;;; elsa-scope.el --- elsa-scope -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 25th March 2017
;; Keywords: languages, lisp

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

;; TODO: this needs to be properly documented because I have no idea
;; how it works anymore.

;;; Code:

(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))
(require 'trinary)

(require 'elsa-variable)

(require 'subr-x)

(defclass elsa-scope nil
  ((vars :initarg :vars
         :initform (make-hash-table)
         :documentation
     "Hash table of variables available in current (lexical) scope.")))

;; (elsa-scope-get-var :: (function ((class elsa-scope) mixed) (or (class elsa-variable) nil)))
(cl-defgeneric elsa-scope-get-var (scope var)
  "Get current state of variable."
  (declare (indent 1)))

(cl-defgeneric elsa-scope-add-var (scope var)
  "Add a binding for a variable.

Bindings introduced by `elsa-scope-add-var' are lexical and must
be cleared when the definig form is closed."
  (declare (indent 1)))

(cl-defgeneric elsa-scope-remove-var (scope var)
  "Remove a binding for variable.

This must only be used to un-do lexical bindings.

If the last update was made by an assignment or the scope is
protected signal an error."
  (declare (indent 1)))

(cl-defgeneric elsa-scope-assign-var (scope var)
  "Record an assignment to variable."
  (declare (indent 1)))

(cl-defgeneric elsa-scope-narrow-var (scope var &optional updater)
  "Record a narrowing of a variable.

UPDATER is a function taking the current binding in SCOPE and the
new VAR and combines their types.  Defaults to simply taking the
new var."
  (declare (indent 1)))

(cl-defgeneric elsa-scope-unassign-var (scope var)
  "Remove all assignments to variable.

If the scope is protected, remove only the assignments done since
the last protection was applied.

Lexical bindings are not undone, use `elsa-scope-remove-var' to
do that."
  (declare (indent 1)))

(defmacro elsa-save-scope (scope &rest body)
  "Protect all variables in SCOPE from unassignment."
  (declare (indent 1)
           (debug (form body)))
  (let ((barrier (make-symbol "elsa--barrier")))
    `(progn
       (elsa-scope-protect ,scope ',barrier)
       ,@body
       (elsa-scope-unassign ,scope ',barrier))))

(defun elsa-scope-protect (scope barrier)
  (let ((vars (oref scope vars)))
    (maphash
     (lambda (k v)
       (puthash k (cons barrier v) vars))
     vars)))

(defun elsa-scope-unassign (scope _barrier)
  (maphash
   (lambda (k _v)
     (elsa-scope-unassign-var scope k))
   (oref scope vars)))

(defun elsa-scope-format (scope)
  "Prettyprint SCOPE."
  (let ((vars (oref scope vars))
        (out nil))
    (maphash
     (lambda (name var-stack)
       (push (format "== Name: %s ==" name) out)
       (push (mapconcat
              (lambda (v)
                (cond
                 ((symbolp v)
                  (symbol-name v))
                 ((and (consp v) (elsa-variable-p (cdr v)))
                  (format "%s: %s"
                          (car v)
                          (elsa-type-describe (elsa-get-type (cdr v)))))
                 (t (error "Can not happen"))))
              var-stack "\n")
             out))
     vars)
    (string-trim (mapconcat 'identity (nreverse out) "\n\n"))))

(defun elsa-scope-print (scope)
  (message "%s" (elsa-scope-format scope)))

;;
;; Implementation

(cl-defmethod elsa-scope-add-var ((this elsa-scope) (variable elsa-variable))
  "Add VARIABLE to current scope."
  (let* ((vars (oref this vars))
         (name (oref variable name))
         (var-stack (gethash name vars)))
    (puthash name (cons (cons 'lexical variable) var-stack) vars)))

(defun elsa-scope--remove-var (scope name)
  (elsa-scope--unassign-var scope name)
  (let* ((vars (oref scope vars))
         (var-stack (gethash name vars)))
    (cl-assert (not (symbolp (car var-stack))))
    (if (cdr var-stack)
        (puthash name (cdr var-stack) vars)
      (remhash name vars))))

(cl-defmethod elsa-scope-remove-var ((this elsa-scope) (variable elsa-variable))
  "Remove VARIABLE from current scope."
  (elsa-scope--remove-var this (oref variable name)))

(cl-defmethod elsa-scope-remove-var ((this elsa-scope) (variables list))
  "Remove VARIABLE from current scope."
  (--each variables (elsa-scope-remove-var this it)))

;; (elsa-scope--get-var :: (function ((class elsa-scope) symbol) (or (class elsa-variable) nil)))
(defun elsa-scope--get-var (scope var-name)
  (let* ((vars (oref scope vars))
         (var-stack (gethash var-name vars)))
    (while (and var-stack (symbolp (car var-stack))) (!cdr var-stack))
    (let ((var (car var-stack)))
      (cond
       ((consp var)
        (cdr var))
       (t var)))))

(cl-defmethod elsa-scope-get-var ((this elsa-scope) (name symbol))
  "Get binding of variable with NAME in THIS scope."
  (elsa-scope--get-var this name))

(cl-defmethod elsa-scope-get-var ((this elsa-scope) (var elsa-variable))
  "Get binding of VAR in THIS scope."
  (elsa-scope--get-var this (oref var name)))

(cl-defmethod elsa-scope-assign-var ((scope elsa-scope) (var elsa-variable))
  (let* ((vars (oref scope vars))
         (name (oref var name))
         (var-stack (gethash name vars))
         (var (elsa-variable
               :name name
               :type (oref var type)
               :assigned (trinary-true))))
    (puthash name (cons (cons 'assign var) var-stack) vars)))

(cl-defmethod elsa-scope-narrow-var ((scope elsa-scope) (var elsa-variable) &optional updater)
  (let* ((vars (oref scope vars))
         (name (oref var name))
         (var-stack (gethash name vars))
         (scope-var (or (elsa-scope-get-var scope name)
                        (elsa-make-variable name (elsa-make-type mixed)))))
    (let ((narrowed-var (funcall (or updater
                                     (lambda (_a b)
                                       (elsa-variable
                                        :name name
                                        :type (elsa-get-type b))))
                                 scope-var var)))
      (puthash name (cons (cons 'narrow-by-return narrowed-var) var-stack) vars))))

(cl-defmethod elsa-scope-narrow-var ((scope elsa-scope) (var list) &optional updater)
  (--each var (elsa-scope-narrow-var scope it updater)))

(defun elsa-scope--unassign-var (scope name)
  (let* ((vars (oref scope vars))
         (var-stack (gethash name vars)))
    (while (and var-stack
                (not (symbolp (car var-stack)))
                (memq (caar var-stack) '(assign narrow-by-return)))
      (!cdr var-stack))
    (when (symbolp (car var-stack))
      (pop var-stack))
    (if var-stack
        (puthash name var-stack vars)
      (remhash name vars))))

(cl-defmethod elsa-scope-unassign-var ((scope elsa-scope) (name symbol))
  (elsa-scope--unassign-var scope name))

(cl-defmethod elsa-scope-unassign-var ((scope elsa-scope) (var elsa-variable))
  (elsa-scope--unassign-var scope (oref var name)))

(defun elsa-variables-merge-to-scope (vars scope)
  "Merge VARS to SCOPE.

Update types of VARS in place or add them if they are missing.
Propagate the assigned/read flags."
  (-each vars
    (lambda (var)
      (-if-let (scope-var (elsa-scope-get-var scope var))
          (progn
            (oset scope-var type (oref var type))
            (oset scope-var assigned (oref var assigned))
            (oset scope-var read (oref var read)))
        (elsa-scope-add-var scope (clone var))))))

(defun elsa-scope-get-assigned-vars (scope)
  (let ((mutated nil))
    (maphash (lambda (_k v)
               (when (eq (car-safe (car v)) 'assign)
                 (push (clone (cdar v)) mutated)))
             (oref scope vars))
    mutated))

(provide 'elsa-scope)
;;; elsa-scope.el ends here
