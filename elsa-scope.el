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

(require 'elsa-variable)
(require 'elsa-reader)

(defclass elsa-scope nil
  ((vars :initarg :vars
         :initform (make-hash-table)
         :documentation
     "Hash table of variables available in current (lexical) scope.")))

(cl-defmethod elsa-scope-add-variable ((this elsa-scope) (variable elsa-variable))
  "Add VARIABLE to current scope."
  (let* ((vars (oref this vars))
         (name (oref variable name))
         (var-stack (gethash name vars)))
    (puthash name (cons variable var-stack) vars)))

(cl-defmethod elsa-scope-add-variable ((this elsa-scope) (form elsa-form-symbol) type)
  "Add VARIABLE to current scope."
  (let* ((name (elsa-form-name form)))
    (elsa-scope-add-variable this (elsa-variable :name name :type type))))

(defun elsa-scope--remove-variable (scope name)
  (let* ((vars (oref scope vars))
         (var-stack (cdr (gethash name vars))))
    (if var-stack
        (puthash name var-stack vars)
      (remhash name vars))))

(cl-defmethod elsa-scope-remove-variable ((this elsa-scope) (variable elsa-variable))
  "Remove VARIABLE from current scope."
  (elsa-scope--remove-variable this (oref variable name)))

(cl-defmethod elsa-scope-remove-variable ((this elsa-scope) (form elsa-form-symbol))
  "Remove VARIABLE from current scope."
  (elsa-scope--remove-variable this (elsa-form-name form)))

(cl-defgeneric elsa-scope-get-var (scope var)
  "Fetch current binding of elsa-variable from elsa-scope.")

(cl-defmethod elsa-scope-get-var ((this elsa-scope) name)
  "Get binding of variable with NAME in THIS scope."
  (let ((vars (oref this vars)))
    (car (gethash name vars))))

(cl-defmethod elsa-scope-get-var ((this elsa-scope) (var elsa-variable))
  "Get binding of VAR in THIS scope."
  (let ((vars (oref this vars)))
    (car (gethash (oref var name) vars))))

(cl-defmethod elsa-scope-get-var ((this elsa-scope) (form elsa-form-symbol))
  "Get binding of FORM in THIS scope."
  (let ((vars (oref this vars)))
    (car (gethash (elsa-form-name form) vars))))

(provide 'elsa-scope)
;;; elsa-scope.el ends here
