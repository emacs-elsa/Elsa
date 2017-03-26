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

;; TODO: add some methods for looking up variables, so we don't
;; directly work with the hashtable
(defclass elsa-scope nil
  ((vars :initarg :vars
         :initform (make-hash-table)
         :documentation
     "Hash table of variables available in current (lexical) scope.")))

(defmethod elsa-scope-add-variable ((this elsa-scope) variable)
  "Add VARIABLE to current scope."
  (unless (elsa-variable-p variable) (error "Variable is not `elsa-variable-p'"))
  (let* ((vars (oref this vars))
         (name (oref variable name))
         (var-stack (gethash name vars)))
    (puthash name (cons variable var-stack) vars)))

(defmethod elsa-scope-remove-variable ((this elsa-scope) variable)
  "Remove VARIABLE from current scope."
  (unless (elsa-variable-p variable) (error "Variable is not `elsa-variable-p'"))
  (let* ((vars (oref this vars))
         (name (oref variable name))
         (var-stack (cdr (gethash name vars))))
    (if var-stack
        (puthash name var-stack vars)
      (remhash name vars))))

(defmethod elsa-scope-get-var ((this elsa-scope) name)
  "Get binding of NAMEd variable in THIS scope."
  (let ((vars (oref this vars)))
    (car (gethash name vars))))

(provide 'elsa-scope)
;;; elsa-scope.el ends here
