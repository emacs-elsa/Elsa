;;; elsa-types.el --- Elsa types -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 23rd March 2017
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

(require 'dash)

(defun elsa-type--get-class-constructor (type)
  "Return constructor information for TYPE.

If the TYPE starts with elsa-type prefix, it is returned as-is.
Otherwise, elsa-type- is prefixed.

If the TYPE is suffixed with `?', the type is recognized as
nullable.

The return value is a plist with :constructor being the
constructor and :nullable a boolean specifying if the type is
nullable.

Constructor is a symbol which is the actual EIEIO type
representing TYPE."
  (let* ((name (split-string (symbol-name type) "?"))
         (class (intern
                 (if (string-prefix-p "elsa-type" (car name))
                     (car name)
                   (concat "elsa-type-"  (car name))))))
    (list :constructor class :nullable (equal (cadr name) ""))))

(defclass elsa-type nil () :abstract t)

(defmethod elsa-type-describe ((this elsa-type))
  "Describe THIS type."
  (declare (elsa-return string))
  (symbol-name (eieio-object-class this)))

(defmethod elsa-type-accept ((this elsa-type) other)
  "Test if THIS type accepts OTHER.

Accepting in this context means that OTHER can be assigned to
THIS."
  (declare (elsa-return bool))
  (cond
   ((elsa-instance-of other this))
   ((and (elsa-sum-type-p other)
         (-all? (lambda (other-type)
                  (elsa-type-accept this other-type))
                (oref other types))))
   (t nil)))

(defmethod elsa-type-restrict-by ((this elsa-type) other)
  (error "Not implemented yet"))

(defmethod elsa-type-make-non-nullable ((this elsa-type))
  (error "Not implemented yet"))

(defclass elsa-type-unbound (elsa-type) ()
  :documentation "Type of an unbound variable.

This is not accepted by any type because we don't know what it is.")

(defmethod elsa-type-describe ((this elsa-type-unbound))
  "unbound")

(defclass elsa-sum-type (elsa-type)
  ((types :type list
          :initarg :types
          :initform nil))
  :documentation "Sum type.

This type is a combination of other types.  It can accept any
type that is accepted by at least one of its summands.")

(defmethod elsa-type-describe ((this elsa-sum-type))
  (mapconcat 'elsa-type-describe (oref this types) " | "))

(cl-defmethod clone ((this elsa-sum-type))
  "Make a deep copy of a sum type."
  (let ((types (-map 'clone (oref this types)))
        (new (cl-call-next-method this)))
    (oset new types types)
    new))

;; TODO: handle sum types and diff types as arguments
(defmethod elsa-sum-type-remove ((this elsa-sum-type) other)
  (unless (elsa-type-child-p other) (error "Other must be `elsa-type-child-p'"))
  (oset this types (--remove (eq (eieio-object-class it)
                                 (eieio-object-class other))
                             (oref this types))))

(defmethod elsa-type-make-non-nullable ((this elsa-sum-type))
  (elsa-sum-type-remove this (elsa-type-nil)))

(defmethod elsa-type-accept ((this elsa-sum-type) other)
  (cond
   ((= 0 (length (oref this types))) nil)
   ((elsa-sum-type-p other)
    (-all? (lambda (ot) (elsa-type-accept this ot)) (oref other types)))
   (t (-any? (lambda (ot) (elsa-type-accept ot other)) (oref this types)))))

(defclass elsa-diff-type (elsa-type)
  ((positive :initform (elsa-sum-type) :initarg :positive)
   (negative :initform (elsa-sum-type) :initarg :negative))
  :documentation "Diff type.

This type is a combination of positive and negative types.  It
can accept any type that is accepted by at least one positive
type and none of the negative types.")

(cl-defmethod clone ((this elsa-diff-type))
  "Make a deep copy of a diff type."
  (let ((positive (clone (oref this positive)))
        (negative (clone (oref this negative)))
        (new (cl-call-next-method this)))
    (oset new positive positive)
    (oset new negative negative)
    new))

(cl-defmethod elsa-type-accept ((this elsa-diff-type) (other elsa-type))
  (and (elsa-type-accept (oref this positive) other)
       (not (elsa-type-accept (oref this negative) other))))

;; (cl-defmethod elsa-diff-type-add-positive ((this elsa-diff-type) (other elsa-type))
;;   (let ((re (clone this :positive :negative))))
;;   (elsa-type-sum (oref this positive) other))

(defmethod elsa-diff-type-remove-positive ((this elsa-diff-type) other)
  (unless (elsa-type-child-p other) (error "Other must be `elsa-type-child-p'"))
  (if (elsa-type-nil-p other)
      (elsa-type-make-non-nullable (oref this positive))
    (elsa-sum-type-remove (oref this positive) other)))

(defclass elsa-type-t (elsa-type) ())

(defclass elsa-type-nil (elsa-type) ())

(defmethod elsa-type-accept ((this elsa-type-nil) other)
  (elsa-type-nil-p other))

(defmethod elsa-type-describe ((this elsa-type-nil))
  "nil")

;; Mixed type is special in that it is always created nullable.  Mixed
;; can also serve as bool type in Emacs Lisp.
(defclass elsa-type-mixed (elsa-type) ())

(defmethod elsa-type-describe ((this elsa-type-mixed))
  "mixed")

(defmethod elsa-type-accept ((this elsa-type-mixed) other)
  (unless (elsa-type-child-p other) (error "Other must be `elsa-type-child-p'"))
  (not (memq (eieio-object-class other)
             '(elsa-type-nil elsa-type-unbound))))

(defclass elsa-type-string (elsa-type) ())

(defclass elsa-type-short-string (elsa-type-string) ())

(defmethod elsa-type-describe ((this elsa-type-string))
  "string")

(defclass elsa-type-buffer (elsa-type) ())

(defmethod elsa-type-describe ((this elsa-type-buffer))
  "buffer")

(defclass elsa-type-number (elsa-type) ())

(defmethod elsa-type-describe ((this elsa-type-number))
  "number")

(defclass elsa-type-int (elsa-type-number) ())

(defmethod elsa-type-describe ((this elsa-type-int))
  "int")

(defclass elsa-type-float (elsa-type-number) ())

(defmethod elsa-type-describe ((this elsa-type-float))
  "float")

(defclass elsa-type-marker (elsa-type) ())

(defmethod elsa-type-describe ((this elsa-type-marker))
  "marker")

(defclass elsa-type-keyword (elsa-type) ())

(defmethod elsa-type-describe ((this elsa-type-keyword))
  "keyword")

(defclass elsa-type-list (elsa-type)
  ((item-type :type elsa-type
              :initarg :item-type
              :initform (elsa-sum-type
                         :types (list (elsa-type-mixed) (elsa-type-nil))))))

(defmethod elsa-type-describe ((this elsa-type-list))
  (format "[%s]" (elsa-type-describe (oref this item-type))))

(defclass elsa-function-type (elsa-type)
  ((args :type list :initarg :args)
   (return :type elsa-type :initarg :return)))

(defmethod elsa-type-describe ((this elsa-function-type))
  (mapconcat 'elsa-type-describe
             (-snoc (oref this args) (oref this return))
             " -> "))

(defclass elsa-generic-type (elsa-type)
  ((label :type symbol :initarg :label)))

(defmethod elsa-type-describe ((this elsa-generic-type))
  (symbol-name (oref this label)))

;; TODO: add a function type, then use it in defuns and variables
;; which are lambdas

(provide 'elsa-types)
;;; elsa-types.el ends here
