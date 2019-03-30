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
(require 'trinary)

(require 'elsa-types)

(cl-defgeneric elsa-type-is-nil (type)
  "Test if TYPE is always nil.

Return trinary logic value.")

(cl-defmethod elsa-type-is-nil ((type elsa-type))
  (if (elsa-type-accept type (elsa-type-nil))
      (if (elsa-type-equivalent-p type (elsa-type-nil))
          (trinary-true)
        (trinary-maybe))
    (trinary-false)))

(cl-defgeneric elsa-type-is-non-nil (type)
  "Test if TYPE is always non-nil.

Return trinary logic value.")

(cl-defmethod elsa-type-is-non-nil ((type elsa-type))
  (if (elsa-type-accept type (elsa-type-nil))
      (if (elsa-type-equivalent-p type (elsa-type-nil))
          (trinary-false)
        (trinary-maybe))
    (trinary-true)))

(defun elsa--make-const-type (value)
  "Construct const type based on VALUE."
  (cond
   ((keywordp value)
    (elsa-const-type :type (elsa-make-type Keyword) :value value))
   ((stringp value)
    (elsa-const-type :type (elsa-make-type String) :value value))
   ((integerp value)
    (elsa-const-type :type (elsa-make-type Int) :value value))
   ((floatp value)
    (elsa-const-type :type (elsa-make-type Float) :value value))
   ((symbolp value)
    (elsa-const-type :type (elsa-make-type Symbol) :value value))
   (t (error "Trying to make a const type out of %S" value))))

(defun elsa--make-union-type (definition)
  (->> (-split-on '| definition)
       (-map 'elsa--make-type)
       (-reduce 'elsa-type-sum)))

(defun elsa--make-type (definition)
  (pcase definition
    (`(Readonly . ,type)
     (elsa-readonly-type :type (elsa--make-type type)))
    (`(Const ,value)
     (elsa--make-const-type value))
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
  `(elsa--make-type ',definition))

(defun elsa--eieio-class-parents-recursive (type)
  "Return all parents of TYPE."
  (cons type
        (-mapcat 'elsa--eieio-class-parents-recursive
                 (-map 'eieio-class-name (eieio-class-parents type)))))

(defun elsa-type-equivalent-p (this other)
  "Test if THIS and OTHER are equivalent types."
  (and (elsa-type-accept (elsa-get-type this) (elsa-get-type other))
       (elsa-type-accept (elsa-get-type other) (elsa-get-type this))))

;; TODO: what is the relationship of `a' and `a?'
(defun elsa-instance-of (this other)
  "Non-nil if THIS is instance of OTHER."
  (let ((this-type
         (eieio-object-class (if (symbolp this) (elsa--make-type (list this)) this)))
        (other-type
         (eieio-object-class (if (symbolp other) (elsa--make-type (list other)) other))))
    (not (null (memq other-type (elsa--eieio-class-parents-recursive this-type))))))

(defun elsa-type-nullable-p (type)
  "Test if TYPE is nullable (i.e. accepts nil)."
  (elsa-type-accept type (elsa-make-type Nil)))

(defun elsa-type-make-nullable (type)
  "Make TYPE nullable."
  (elsa-type-sum type (elsa-make-type Nil)))

(defun elsa-type-make-non-nullable (type)
  "Make TYPE non-nullable."
  (elsa-type-diff type (elsa-make-type Nil)))

(cl-defgeneric elsa-type-normalize (type)
  "Normalize TYPE to its most simplest form.")

(cl-defgeneric elsa-type-normalize ((this elsa-type))
  this)

(cl-defmethod elsa-type-normalize ((this elsa-sum-type))
  "Normalize a sum type."
  (let ((types (--remove (elsa-type-accept (elsa-make-type Empty) it)
                         (oref this types))))
    (cond
     ((not types)
      (elsa-type-empty))
     ((= 1 (length types))
      (car types))
     (t (elsa-sum-type :types types)))))

(cl-defmethod elsa-type-normalize ((this elsa-intersection-type))
  "Normalize a sum type."
  (let ((types (--remove (elsa-type-accept it (elsa-make-type Mixed))
                         (oref this types))))
    (cond
     ((= 1 (length types))
      (car types))
     (t (elsa-intersection-type :types types)))))

(cl-defmethod elsa-type-normalize ((this elsa-diff-type))
  "Normalize a diff type."
  (let ((pos (oref this positive))
        (neg (oref this negative)))
    (cond
     ((elsa-type-equivalent-p pos neg)
      (elsa-type-empty))
     ((elsa-type-equivalent-p neg (elsa-type-empty))
      (clone pos))
     (t this))))

(cl-defgeneric elsa-type-intersect (this other)
  "Return the intersection of THIS and OTHER type.

An intersection only accepts what both THIS and OTHER accept.")

(cl-defgeneric elsa-type-sum (this other)
  "Return the sum of THIS and OTHER type.

A sum accept anything that either THIS or OTHER accepts.")

(cl-defmethod elsa-type-sum ((_this elsa-type-empty) (other elsa-type))
  other)

(cl-defmethod elsa-type-sum ((this elsa-type) (_other elsa-type-empty))
  this)

(cl-defmethod elsa-type-sum ((this elsa-type) (other elsa-type))
  (cond
   ((or (and (elsa-type-nil-p this) (elsa-type-t-p other))
        (and (elsa-type-t-p this) (elsa-type-nil-p other)))
    (elsa-type-bool))
   ((elsa-type-accept this other)
    (clone this))
   ((elsa-type-accept other this)
    (clone other))
   (t (let ((sum (elsa-sum-type :types (list this))))
        (elsa-type-sum sum other)))))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-sum-type))
  (elsa-type-normalize
   (let ((new nil))
     (-each (oref other types)
       (lambda (type)
         (unless (elsa-type-accept this type)
           (push (clone type) new))))
     (elsa-sum-type :types (-concat (oref this types) (nreverse new))))))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-type))
  (elsa-type-normalize
   (if (elsa-type-accept this other)
       (clone this)
     (elsa-sum-type :types (-snoc (oref this types) (clone other))))))

(cl-defmethod elsa-type-sum ((this elsa-diff-type) (other elsa-type))
  (elsa-type-normalize
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
    (if (and (elsa-const-type-p other)
             (elsa-type-accept this (oref other type)))
        (elsa-diff-type :positive (clone this)
                        :negative (clone other))
      (clone this))))

(cl-defmethod elsa-type-diff ((_this elsa-type-mixed) other)
  "Mixed is one with everything, so we need to subtract OTHER from the world."
  (elsa-type-normalize (elsa-diff-type :negative (clone other))))

(cl-defmethod elsa-type-diff ((this elsa-type-mixed) (other elsa-diff-type))
  "Mixed without (Mixed without something) is something.

This uses the rule that A \ (A \ B) = A ∩ B where A is
everything (Mixed)."
  (let ((pos (oref other positive))
        (neg (oref other negative)))
    (if (elsa-type-equivalent-p this pos)
        (clone neg)
      (elsa-diff-type :negative (clone other)))))

(cl-defmethod elsa-type-diff ((this elsa-type-number) (_other elsa-type-int))
  "Number without int must be float."
  (cond
   ((elsa-type-int-p this)
    (elsa-type-empty))
   ((elsa-type-float-p this)
    this)
   (t
    (elsa-type-float))))

(cl-defmethod elsa-type-diff ((this elsa-type-number) (_other elsa-type-float))
  "Number without float must by int."
  (cond
   ((elsa-type-int-p this)
    this)
   ((elsa-type-float-p this)
    (elsa-type-empty))
   (t
    (elsa-type-int))))

(cl-defmethod elsa-type-diff ((_this elsa-type-bool) (_other elsa-type-t))
  "Bool without T is Nil."
  (elsa-type-nil))

(cl-defmethod elsa-type-diff ((_this elsa-type-bool) (_other elsa-type-nil))
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
    (elsa-type-normalize
     (elsa-sum-type :types (nreverse new)))))

(cl-defmethod elsa-type-diff ((this elsa-diff-type) other)
  "This uses the rule that (A \ B) \ C = A \ (B ∪ C)"
  (elsa-type-normalize
   (elsa-diff-type :positive (clone (oref this positive))
                   :negative (elsa-type-sum (oref this negative) other))))

(cl-defgeneric elsa-type-intersect (this other)
  "Return the intersection of THIS and OTHER.

The intersection type only accepts those types which are both
THIS and OTHER at the same time.")

(cl-defmethod elsa-type-intersect ((this elsa-type) (other elsa-type))
  (cond
   ((elsa-type-accept this other)
    (clone other))
   ((elsa-type-accept other this)
    (clone this))
   (t (elsa-type-empty))))

(cl-defmethod elsa-type-intersect ((this elsa-sum-type) (other elsa-type))
  (let ((types (oref this types)))
    (elsa-type-normalize
     (elsa-sum-type :types
       (-map (lambda (type) (elsa-type-intersect type other)) types)))))

(cl-defmethod elsa-type-intersect ((this elsa-diff-type) (other elsa-type))
  (let ((pos (oref this positive))
        (neg (oref this negative)))
    (elsa-type-normalize
     (elsa-type-diff (elsa-type-intersect pos other) (clone neg)))))

(cl-defmethod elsa-type-intersect ((this elsa-intersection-type) (other elsa-type))
  (if (-any? (lambda (type) (elsa-type-accept other type)) (oref this types))
      (clone this)
    (elsa-intersection-type :types (-snoc (oref this types) other))))

(provide 'elsa-type-helpers)
;;; elsa-type-helpers.el ends here
