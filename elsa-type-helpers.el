;;; elsa-type-helpers.el --- Elsa type helpers -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 6th June 2017
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

(require 'cl-generic)
(require 'eieio)

(require 'dash)

(require 'elsa-types)

(defun elsa--make-type (definition)
  "Return instance of class representing DEFINITION."
  (pcase definition
    ((and (pred vectorp) vector)
     (let ((definition (append vector nil)))
       (pcase definition
         (`(&or . ,types)
          (apply 'elsa-make-type types))
         (`(,type) (elsa-type-list :item-type (elsa-make-type type))))))
    (`(cons ,first ,second)
     (elsa-type-cons :car-type (elsa-make-type first)
                     :cdr-type (elsa-make-type second)))
    (`sum
     (make-instance 'elsa-sum-type))
    (`integer
     (elsa-type-int))
    (`number-or-marker?
     (elsa-type-make-nullable (elsa-make-type 'number-or-marker)))
    (`number-or-marker
     (elsa-type-sum (elsa-make-type 'number) (elsa-make-type 'marker)))
    (_
     (-let* (((&plist :constructor c :nullable n)
              (elsa-type--get-class-constructor definition))
             (instance (make-instance c))
             (nullable (or n (eq c 'elsa-type-mixed))))
       (if nullable (elsa-type-make-nullable instance) instance)))))

(defun elsa-make-type (&rest definitions)
  "Make a type according to DEFINITIONS.

DEFINITIONS is a list of DEFINITION.

DEFINITION can be a simple type such as 'int or 'string or a
special FORM.

FORM can be one of:
- [&or type1 type2 ...] -> Return the sum of type1, type2, ...
"
  (if (= (length definitions) 1)
      (elsa--make-type (car definitions))
    (-reduce-r 'elsa-type-sum (-map 'elsa--make-type definitions))))

(defun elsa--eieio-class-parents-recursive (type)
  "Return all parents of TYPE."
  (cons type
        (-mapcat 'elsa--eieio-class-parents-recursive
                 (-map 'eieio-class-name (eieio-class-parents type)))))

;; TODO: what is the relationship of `a' and `a?'
(defun elsa-instance-of (this other)
  "Non-nil if THIS is instance of OTHER."
  (let ((this-type
         (if (symbolp this)
             (plist-get (elsa-type--get-class-constructor this) :constructor)
           (eieio-object-class this)))
        (other-type
         (if (symbolp other)
             (plist-get (elsa-type--get-class-constructor other) :constructor)
           (eieio-object-class other))))
    (not (null
          (memq other-type (elsa--eieio-class-parents-recursive this-type))))))

(cl-defmethod elsa-type-nullable-p ((this elsa-type))
  (elsa-type-accept this (elsa-make-type 'nil)))

(cl-defmethod elsa-type-make-nullable ((this elsa-type))
  (elsa-type-sum this (elsa-make-type 'nil)))

(defun elsa-type-sum-normalize (sum)
  "Normalize a sum type.

If the SUM only contains one type, return that type directly."
  (let ((types (oref sum types)))
    (if (= 1 (length types))
        (car types)
      sum)))

;; TODO:
(defun elsa-type-diff-normalize (diff)
  "Normalize a diff type.

If the positive or negative types of the DIFF type contain only
one type make them that type directly instead of a sum."
  diff)

(cl-defgeneric elsa-type-intersect (this other)
  "Return the intersection of THIS and OTHER type.

An intersection only accepts what both THIS and OTHER accept.")

(cl-defgeneric elsa-type-sum (this other)
  "Return the sum of THIS and OTHER type.

A sum accept anything that either THIS or OTHER accepts.")

(cl-defmethod elsa-type-sum ((this elsa-type) (other elsa-type))
  (let ((sum (elsa-sum-type :types (list this))))
    (elsa-type-sum sum other)))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-sum-type))
  (elsa-type-sum-normalize
   (let ((re (oref this types)))
     (-each (oref other types)
       (lambda (type)
         (unless (elsa-type-accept this type)
           (push (clone type) re))))
     (elsa-sum-type :types re))))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-type))
  (elsa-type-sum-normalize
   (if (elsa-type-accept this other)
       (clone this)
     (elsa-sum-type :types (cons (clone other) (oref this types))))))

(cl-defmethod elsa-type-sum ((this elsa-diff-type) (other elsa-type))
  (elsa-type-diff-normalize
   (if (elsa-type-accept this other)
       (clone this)
     (let ((new (clone this)))
       (oset new positive (elsa-type-sum (oref new positive) (clone other)))
       new))))

(cl-defgeneric elsa-type-diff (this other)
  "Return the difference of THIS without OTHER.

The diff type only accepts those types accepted by THIS which are
not accepted by OTHER.")


;; (cl-defgeneric elsa-type-diff ((this elsa-type) (other elsa-type))
;;   (elsa-type-diff-normalize
;;    (let ((diff (elsa-diff-type :positive (list this))))))
;;   )

(provide 'elsa-type-helpers)
;;; elsa-type-helpers.el ends here
