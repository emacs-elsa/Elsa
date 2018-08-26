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

;;; Code:

(require 'eieio)
(require 'trinary)

(require 'elsa-variable)
(require 'elsa-reader)

(require 'subr-x)

(defclass elsa-scope nil
  ((vars :initarg :vars
         :initform (make-hash-table)
         :documentation
     "Hash table of variables available in current (lexical) scope.")))

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

(cl-defgeneric elsa-scope-unassign-var (scope var)
  "Remove all assignments to variable.

If the scope is protected, remove only the assignments done since
the last protection was applied.

Lexical bindings are not undone, use `elsa-scope-remove-var' to
do that."
  (declare (indent 1)))

(defun elsa-scope-protect (scope barrier)
  (let ((vars (oref scope vars)))
    (maphash
     (lambda (k v)
       (puthash k (cons barrier v) vars))
     vars)))

(defun elsa-scope-unassign (scope barrier)
  (maphash
   (lambda (k _v)
     (elsa-scope-unassign-var scope k))
   (oref scope vars)))

;;
;; Implementation

(cl-defmethod elsa-scope-add-variable ((this elsa-scope) (variable elsa-variable))
  "Add VARIABLE to current scope."
  (let* ((vars (oref this vars))
         (name (oref variable name))
         (var-stack (gethash name vars)))
    (puthash name (cons variable var-stack) vars)))

(defun elsa-scope--remove-variable (scope name)
  (elsa-scope--unassign-var scope name)
  (let* ((vars (oref scope vars))
         (var-stack (gethash name vars)))
    (cl-assert (not (symbolp (car var-stack))))
    (if (cdr var-stack)
        (puthash name (cdr var-stack) vars)
      (remhash name vars))))

(cl-defmethod elsa-scope-remove-variable ((this elsa-scope) (variable elsa-variable))
  "Remove VARIABLE from current scope."
  (elsa-scope--remove-variable this (oref variable name)))

(cl-defmethod elsa-scope-remove-variable ((this elsa-scope) (form elsa-form-symbol))
  "Remove VARIABLE from current scope."
  (elsa-scope--remove-variable this (elsa-form-name form)))

(defun elsa-scope--get-var (scope var-name)
  (let* ((vars (oref scope vars))
         (var-stack (gethash var-name vars)))
    (while (and var-stack (symbolp (car var-stack))) (!cdr var-stack))
    (car var-stack)))

(cl-defmethod elsa-scope-get-var ((this elsa-scope) name)
  "Get binding of variable with NAME in THIS scope."
  (elsa-scope--get-var this name))

(cl-defmethod elsa-scope-get-var ((this elsa-scope) (var elsa-variable))
  "Get binding of VAR in THIS scope."
  (elsa-scope--get-var this (oref var name)))

(cl-defmethod elsa-scope-get-var ((this elsa-scope) (form elsa-form-symbol))
  "Get binding of FORM in THIS scope."
  (elsa-scope--get-var this (elsa-form-name form)))

(cl-defmethod elsa-scope-assign-var ((scope elsa-scope) (var elsa-variable))
  (let* ((vars (oref scope vars))
         (name (oref var name))
         (var-stack (gethash name vars)))
    (puthash name (cons (elsa-variable :name name
                                       :type (oref var type)
                                       :assigned (trinary-true))
                        (cons 'setq var-stack)) vars)))

(defun elsa-scope--unassign-var (scope name)
  (let* ((vars (oref scope vars))
         (var-stack (gethash name vars)))
    (while (and var-stack
                (not (symbolp (car var-stack)))
                (eq (cadr var-stack) 'setq))
      (setq var-stack (cddr var-stack)))
    (when (symbolp (car var-stack))
      (pop var-stack))
    (if var-stack
        (puthash name var-stack vars)
      (remhash name vars))))

(cl-defmethod elsa-scope-unassign-var ((scope elsa-scope) name)
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
        (elsa-scope-add-variable scope (clone var))))))

(defun elsa-scope-get-assigned-vars (scope)
  (let ((mutated nil))
    (maphash (lambda (_k v)
               (when (and (eq (cadr v) 'setq)
                          (not (symbolp (car v))))
                 (push (clone (car v)) mutated)))
             (oref scope vars))
    mutated))

(provide 'elsa-scope)
;;; elsa-scope.el ends here
