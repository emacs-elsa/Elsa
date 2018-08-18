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

(defun elsa--make-union-type (definition)
  (->> (-split-on '| definition)
       (-map 'elsa--make-type)
       (-reduce 'elsa-type-sum)))

(defun elsa--make-type (definition)
  (pcase definition
    (`(Cons) ;; mixed cons by default
     (elsa-type-cons :car-type (elsa-type-mixed)
                     :cdr-type (elsa-type-mixed)))
    (`(Vector) ;; mixed vector by default
     (elsa-type-vector :item-type (elsa-type-mixed)))
    (`(List) ;; mixed list by default
     (let* ((item-type (elsa-type-mixed))
            (list-type (elsa-type-list :item-type item-type)))
       (oset list-type car-type item-type)
       (oset list-type cdr-type item-type)
       list-type))
    ((and def (guard (vectorp def)))
     (let* ((item-type (elsa--make-type (append def nil)))
            (list-type (elsa-type-list :item-type item-type)))
       (oset list-type car-type item-type)
       (oset list-type cdr-type item-type)
       list-type))
    ((and `(,arg) (guard (and (atom arg)
                              (not (vectorp arg)))))
     (let* ((type-name (downcase (symbol-name arg)))
            (variadic (when (string-suffix-p "..." type-name)
                        (setq type-name (substring type-name 0 -3))
                        t))
            (nullable (when (string-suffix-p "?" type-name)
                        (setq type-name (substring type-name 0 -1))
                        t))
            (constructor (intern (concat "elsa-type-" type-name))))
       (cond
        ((functionp constructor)
         (let* ((type (funcall constructor))
                (type (if nullable
                          (elsa-type-make-nullable type)
                        type))
                (type (if variadic
                          (elsa-variadic-type :item-type type)
                        type)))
           type))
        (t (error "Unknown type %s" type-name)))))
    ((and `(,arg . nil))
     (elsa--make-type arg))
    ((and def (guard (memq '-> def)))
     (let* ((args (-split-on '-> def))
            (parameters (-map 'elsa--make-type args)))
       (elsa-function-type
        :args (-butlast parameters)
        :return (-last-item parameters))))
    ((and def (guard (memq '| def)))
     (elsa--make-union-type def))
    (`(Cons ,a ,b)
     (elsa-type-cons :car-type (elsa--make-type (list a))
                     :cdr-type (elsa--make-type (list b))))
    (`(List ,a)
     (let* ((item-type (elsa--make-type (list a)))
            (list-type (elsa-type-list :item-type item-type)))
       (oset list-type car-type item-type)
       (oset list-type cdr-type item-type)
       list-type))
    (`(Variadic ,a)
     (let* ((item-type (elsa--make-type (list a)))
            (variadic-type (elsa-variadic-type :item-type item-type)))
       variadic-type))
    (`(Maybe ,a)
     (let* ((item-type (elsa--make-type (list a))))
       (elsa-type-make-nullable item-type)))
    (`(Vector ,a)
     (let* ((item-type (elsa--make-type (list a)))
            (vector-type (elsa-type-vector :item-type item-type)))
       vector-type))))

(defmacro elsa-make-type (&rest definition)
  "Make a type according to DEFINITION.


The grammar is as follows (in eBNF):

<TYPE> ::= <BASE>
         | '(', <TYPE>, ')'
         | <CONSTRUCTOR>, {<TYPE>}
         | <TYPE>, {'|', <TYPE>}
         | <TYPE>, {'->', <TYPE>}

<BASE> ::= 'String' | 'Int' | 'Float' | 'Marker' | 'Buffer'
"
  (elsa--make-type definition))

(defun elsa--eieio-class-parents-recursive (type)
  "Return all parents of TYPE."
  (cons type
        (-mapcat 'elsa--eieio-class-parents-recursive
                 (-map 'eieio-class-name (eieio-class-parents type)))))

(defun elsa-type-equivalent-p (this other)
  "Test if THIS and OTHER are equivalent types."
  (and (elsa-type-accept this other)
       (elsa-type-accept other this)))

;; TODO: what is the relationship of `a' and `a?'
(defun elsa-instance-of (this other)
  "Non-nil if THIS is instance of OTHER."
  (let ((this-type
         (eieio-object-class (if (symbolp this) (elsa--make-type (list this)) this)))
        (other-type
         (eieio-object-class (if (symbolp other) (elsa--make-type (list other)) other))))
    (not (null (memq other-type (elsa--eieio-class-parents-recursive this-type))))))

(cl-defmethod elsa-type-nullable-p ((this elsa-type))
  (elsa-type-accept this (elsa-make-type Nil)))

(cl-defmethod elsa-type-make-nullable ((this elsa-type))
  (elsa-type-sum this (elsa-make-type Nil)))


;; TODO: turn into a generic
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

(cl-defmethod elsa-type-sum ((this elsa-type-empty) (other elsa-type))
  other)

(cl-defmethod elsa-type-sum ((this elsa-type) (other elsa-type-empty))
  this)

(cl-defmethod elsa-type-sum ((this elsa-type) (other elsa-type))
  (let ((sum (elsa-sum-type :types (list this))))
    (elsa-type-sum sum other)))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-sum-type))
  (elsa-type-sum-normalize
   (let ((new nil))
     (-each (oref other types)
       (lambda (type)
         (unless (elsa-type-accept this type)
           (push (clone type) new))))
     (elsa-sum-type :types (-concat (oref this types) (nreverse new))))))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-type))
  (elsa-type-sum-normalize
   (if (elsa-type-accept this other)
       (clone this)
     (elsa-sum-type :types (-snoc (oref this types) (clone other))))))

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

(cl-defmethod elsa-type-diff ((this elsa-type) other)
  "Any base type without another is the same type, there is no intersection."
  (if (elsa-type-accept other this)
      (elsa-type-empty)
    (clone this)))

(cl-defmethod elsa-type-diff ((this elsa-type-mixed) other)
  "Mixed is one with everything, so we need to subtract OTHER from the world."
  (elsa-type-diff-normalize (elsa-diff-type :negative (clone other))))

(cl-defmethod elsa-type-diff ((this elsa-type-number) (other elsa-type-int))
  "Number without int must be float."
  (cond
   ((elsa-type-int-p this)
    (elsa-type-empty))
   ((elsa-type-float-p this)
    this)
   (t
    (elsa-type-float))))

(cl-defmethod elsa-type-diff ((this elsa-type-number) (other elsa-type-float))
  "Number without float must by int."
  (cond
   ((elsa-type-int-p this)
    this)
   ((elsa-type-float-p this)
    (elsa-type-empty))
   (t
    (elsa-type-int))))

(cl-defmethod elsa-type-diff ((this elsa-type-bool) (other elsa-type-t))
  "Bool without T is Nil."
  (elsa-type-nil))

(cl-defmethod elsa-type-diff ((this elsa-type-bool) (other elsa-type-nil))
  "Bool without NIL is T."
  (elsa-type-t))

(cl-defmethod elsa-type-diff ((this elsa-type) (other elsa-sum-type))
  "A difference of a type and sum is THIS minus all the summed types."
  (let ((new (clone this)))
    (-reduce-from 'elsa-type-diff new (oref other types))))

(cl-defmethod elsa-type-diff ((this elsa-sum-type) other)
  "A difference of sum and some other type can only reduce the arguments of the sum."
  (let (new)
    (-each (oref this types)
      (lambda (type)
        (let ((diff-type (elsa-type-diff type other)))
          (unless (elsa-type-empty-p diff-type)
            (push (clone diff-type) new)))))
    (elsa-type-sum-normalize
     (elsa-sum-type :types (nreverse new)))))

(cl-defmethod elsa-type-diff ((this elsa-diff-type) other)
  (elsa-type-diff-normalize
   (elsa-diff-type :positive (elsa-type-diff (oref this positive) other)
                   :negative (elsa-type-sum (oref this negative) other))))

(provide 'elsa-type-helpers)
;;; elsa-type-helpers.el ends here
