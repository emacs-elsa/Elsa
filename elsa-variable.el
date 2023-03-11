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
(eval-and-compile (setq eieio-backward-compatibility nil))

(require 'trinary)
(require 'dash)

(require 'elsa-methods)
(require 'elsa-type)
(require 'elsa-types-simple)

;; TODO: maybe move the acceptance resolution into an expression type?
(defclass elsa-variable nil
  ((name
    :type symbol
    :initarg :name
    :documentation "Name of the variable, as symbol.")
   (type
    :type elsa-type
    :initarg :type
    :initform (elsa-type-mixed)
    :documentation "Type of this variable.

This is the type that the symbol form representing this variable
will assume during analysis.")
   (certainty
    :type trinary
    :initarg :certainty
    :initform (trinary-true)
    :documentation "Are we sure this type is what we say it is?

Sometimes during narrowing we might tentatively narrow variable to a
type, but in the else branch it might not actually be certain that the
narrowing took place.

for example (if (and (stringp x) (random-function)) x x)

In any case, x is a string in the then body, because both conditions
must have been simultaneously true.

But for the else body:

(stringp x) (random-function) |  X  must be
    t               t         |   string
------------------------------+-------------
   nil              t         |
    t              nil        |    mixed
   nil             nil        |

Only the first row is certain, because we know for a fact that both
conditions must have been true and the else body never executes.

In the rows two, three and four, in the else body we don't know which
combination actually happened and so we can't definitely rule out the
case, where x still could be a string (row three).

However, if the second function always returns non-nil, we can
eliminate rows three and four and then we can narrow the else body
type of x to not string.

The example of such a form is (and (string x) (concat \"\" x)).")
   (assigned
    :type trinary
    :initarg :assigned
    :initform (trinary-false)
    :documentation
    "Non-nil if this variable was assigned in the current scope.")
   (read
    :type trinary
    :initarg :read
    :initform (trinary-false)
    :documentation
    "Non-nil if this variable was read in the current scope."))
  :documentation "A lexical variable.")

(cl-defmethod elsa-get-type ((this elsa-variable))
  (oref this type))

(cl-defmethod elsa-get-name ((this elsa-variable))
  (oref this name))

(cl-defmethod elsa-tostring ((this elsa-variable))
  (format "(%s %s)"
          (elsa-get-name this)
          (elsa-tostring (elsa-get-type this))))

(cl-defmethod cl-print-object ((this elsa-variable) stream)
  (princ (concat "#<elsa-variable " (elsa-tostring this) ">") stream))

(cl-defmethod elsa-type-make-non-nullable ((this elsa-variable))
  (oset this type (elsa-type-make-non-nullable (oref this type)))
  this)

(defun elsa-make-variable (name type)
  "Make variable NAME with TYPE."
  (elsa-variable :name name :type type))

;; TODO: propagate assigned/read
(cl-defmethod elsa-variable-diff ((this elsa-variable) (other elsa-variable))
  (elsa-variable :name (oref this name)
                 :type (if (trinary-true-p (oref other certainty))
                           (elsa-type-diff (oref this type) (oref other type))
                         (oref this type))))

;; TODO: propagate assigned/read
(cl-defmethod elsa-variable-intersect ((this elsa-variable) (other elsa-variable))
  (elsa-variable :name (oref this name)
                 :type (elsa-type-intersect (oref this type) (oref other type))
                 :certainty (trinary-and (oref this certainty) (oref other certainty))))

;; TODO: propagate assigned/read
(cl-defmethod elsa-variable-sum ((this elsa-variable) (other elsa-variable))
  (elsa-variable :name (oref this name)
                 :type (elsa-type-sum (oref this type) (oref other type))
                 :certainty (trinary-or (oref this certainty) (oref other certainty))))

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
