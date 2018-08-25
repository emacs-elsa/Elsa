;;; elsa-variable.el --- elsa-variable -*- lexical-binding: t -*-

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

;; TODO: maybe move the acceptance resolution into an expression type?
(defclass elsa-variable nil
  ((name :initarg :name)
   (type :initarg :type
         :documentation "Type of this variable.

This is the type that the symbol form representing this variable
will assume during analysis."))
  :documentation "A lexical variable")

;; TODO: propagate assigned/read
(cl-defmethod elsa-variable-diff ((this elsa-variable) other)
  (elsa-variable :name (oref this name)
                 :type (elsa-type-diff (oref this type) other)))

;; TODO: propagate assigned/read
(cl-defmethod elsa-variable-diff ((this elsa-variable) (other elsa-variable))
  (elsa-variable :name (oref this name)
                 :type (elsa-type-diff (oref this type) (oref other type))))

;; TODO: propagate assigned/read
(cl-defmethod elsa-variable-intersect ((this elsa-variable) (other elsa-variable))
  (elsa-variable :name (oref this name)
                 :type (elsa-type-intersect (oref this type) (oref other type))))

;; TODO: propagate assigned/read
(cl-defmethod elsa-variable-sum ((this elsa-variable) (other elsa-variable))
  (elsa-variable :name (oref this name)
                 :type (elsa-type-sum (oref this type) (oref other type))))

(cl-defmethod elsa-type-diff ((this elsa-variable) other)
  (elsa-type-diff (oref this type) other))

(cl-defmethod elsa-type-diff ((this elsa-variable) (other elsa-variable))
  (elsa-type-diff (oref this type) (oref other type)))

(cl-defmethod elsa-type-intersect ((this elsa-variable) (other elsa-variable))
  (elsa-type-intersect (oref this type) (oref other type)))

(cl-defmethod elsa-type-sum ((this elsa-variable) (other elsa-variable))
  (elsa-type-sum (oref this type) (oref other type)))

(defun elsa-variables-group-and-sum (vars)
  "Take a list of variables VARS, group them by name and sum the types."
  (let ((groups (--group-by (oref it name) vars)))
    (-map (lambda (group) (-reduce 'elsa-variable-sum (cdr group))) groups)))

(defun elsa-variables-group-and-intersect (vars)
  "Take a list of variables VARS, group them by name and intersect the types."
  (let ((groups (--group-by (oref it name) vars)))
    (-map (lambda (group) (-reduce 'elsa-variable-intersect (cdr group))) groups)))

(provide 'elsa-variable)
;;; elsa-variable.el ends here
