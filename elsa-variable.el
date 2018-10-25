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
(require 'trinary)

;; TODO: maybe move the acceptance resolution into an expression type?
(defclass elsa-variable nil
  ((name :initarg :name)
   (type :initarg :type
         :initform (elsa-type-mixed)
         :documentation "Type of this variable.

This is the type that the symbol form representing this variable
will assume during analysis.")
   (assigned :initarg :assigned
             :initform (trinary-false)
             :type trinary
             :documentation
     "Non-nil if this variable was assigned in the current scope.")
   (read :initarg :read
         :initform (trinary-false)
         :type trinary
         :documentation
     "Non-nil if this variable was read in the current scope."))
  :documentation "A lexical variable")

(cl-defmethod elsa-get-type ((this elsa-variable))
  (oref this type))

(cl-defmethod elsa-get-name ((this elsa-variable))
  (oref this name))

(defun elsa-make-variable (name type)
  "Make variable NAME with TYPE."
  (elsa-variable :name name :type type))

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

(defun elsa--variables-partition (first second)
  "Partition variables in FIRST and SECOND group to those in first, second or both."
  (let (first-group second-group both-group)
    (-each first
      (lambda (first-var)
        (-if-let (second-var (--first (eq (oref first-var name) (oref it name)) second))
            (push (elsa-variable :name (oref first-var name)
                                 :type (elsa-type-sum first-var second-var)
                                 :assigned (trinary-happened
                                            (oref first-var assigned)
                                            (oref second-var assigned))
                                 :read (trinary-happened
                                        (oref first-var read)
                                        (oref second-var read)))
                  both-group)
          (push (clone first-var) first-group))))
    (-each second
      (lambda (second-var)
        (unless (--any-p (eq (oref second-var name) (oref it name)) both-group)
          (push (clone second-var) second-group))))
    (list (nreverse first-group) (nreverse second-group) (nreverse both-group))))

(defun elsa-variables-group-and-sum (vars)
  "Take list of VARS, group by name and sum the types."
  (let ((groups (--group-by (oref it name) vars)))
    (-map (lambda (group) (-reduce 'elsa-variable-sum (cdr group))) groups)))

(defun elsa-variables-group-and-intersect (vars)
  "Take list of VARS, group by name and intersect the types."
  (let ((groups (--group-by (oref it name) vars)))
    (-map (lambda (group) (-reduce 'elsa-variable-intersect (cdr group))) groups)))

(provide 'elsa-variable)
;;; elsa-variable.el ends here
