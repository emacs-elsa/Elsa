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
(eval-and-compile (setq eieio-backward-compatibility nil))
(require 'cl-macs)

(require 'trinary)
(require 'dash)

(require 'elsa-type)
(require 'elsa-types-simple)
(require 'elsa-methods)
(require 'elsa-explainer)
(require 'elsa-structure-slot)

(defclass elsa-type--structured nil
  (
   ;; (slots :: (hash-table symbol (class elsa-structure-slot)))
   (slots
    :type hash-table
    :initarg :slots
    :initform (make-hash-table))
   (extends
    :type (list-of symbol)
    :initarg :extends
    :initform nil))
  :documentation "Structured Elsa type with named slots.

This class abstracts the management of slots on an object-like type.
These include structured types like classes or structures as well as
ad-hoc types like plists.")

(cl-defmethod elsa-get-slots ((this elsa-type--structured))
  (append
   (hash-table-values (oref this slots))
   (->> (oref this extends)
        (-keep (lambda (iface) (get iface 'elsa-interface)))
        (-mapcat #'elsa-get-slots))))

(cl-defmethod elsa-get-slot ((this elsa-type--structured) (name symbol))
  (or (gethash name (oref this slots))
      (-some
       (lambda (iface)
         (when-let ((iface-type (get iface 'elsa-interface)))
           (elsa-get-slot iface-type name)))
       (oref this extends))))

(defun elsa-type--check-slots (this other relation explainer)
  "Check that slots on THIS are compatible with slots on OTHER using RELATION.

RELATION can be any relation which compares two types and returns
bool.  Most common relations are `elsa-type-accept' and
`elsa-type-assignable-p'"
  (catch 'done
    (mapc
     (lambda (slot)
       (let ((name (oref slot name)))
         (unless (-if-let* ((other-slot (elsa-get-slot other name)))
                     (elsa-with-explainer explainer
                       (elsa--fmt-explain-types-of-slot-0-do-not-match name)
                       (funcall relation
                                (oref slot type)
                                (oref other-slot type)
                                explainer))
                   (elsa-explain explainer
                     elsa--fmt-explain-slot-0-not-found-on-1
                     name (elsa-tostring other))
                   nil)
           (throw 'done nil))))
     (elsa-get-slots this))
    t))

(cl-defmethod elsa-tostring ((this elsa-type))
  (elsa-type-describe this))

(cl-defmethod cl-print-object ((this elsa-type) stream)
  (princ (concat "#<elsa-type " (elsa-tostring this) ">") stream))

(defclass elsa-composite-type nil ()
  :abstract t
  :documentation "Composite type holding other types.

This type is a wrapper around one or more other types and
modifies their behaviour.")

;; (elsa-type-accept :: (function (mixed mixed mixed) bool))
(cl-defgeneric elsa-type-accept (_this _other &optional _explainer)
  "Check if THIS accepts OTHER.

This means OTHER is assignable to THIS.

If EXPLAINER is non-nil, return instead a recursive explanation why
one type can not accept another.

This method only performs soundness analysis.  To account for the
special handling of `elsa-type-mixed', use
`elsa-type-assignable-p'.")

(cl-defgeneric elsa-type-is-accepted-by (this other &optional explainer)
  "Check if THIS is accepted by OTHER.

This means THIS is assignable to OTHER.

This method is used to unpack composite types and perform a
\"double dispatch\" when testing one composite type accepting
another, because there is a many-to-many relationship."
  (elsa-type-accept other this explainer))

;; TODO: what is the relationship of `a' and `a?'
;; (elsa-instance-of :: (function ((class elsa-type) (class elsa-type)) bool))
(defun elsa-instance-of (this other)
  "Non-nil if THIS is instance of OTHER."
  (object-of-class-p this (eieio-object-class other)))

;; Mixed type is special in that it is always created nullable.  Mixed
;; can also serve as bool type in Emacs Lisp.

(defun elsa-type-assignable-p (this other &optional explainer)
  "Check if THIS accepts OTHER.

Uses special rules for `elsa-type-mixed'.

- Mixed accepts anything except unbound.
- Mixed is accepted by anything."
  (or (and (elsa-type-mixed-p this)
           (not (elsa-type-unbound-p other)))
      (elsa-type-mixed-p other)
      (elsa-type-accept this other explainer)))

(cl-defmethod elsa-get-type ((_this null))
  (elsa-type-unbound))

(cl-defmethod elsa-get-type ((this elsa-type))
  this)

(cl-defmethod elsa-type-describe ((this elsa-type))
  "Describe THIS type."
  (symbol-name (eieio-object-class this)))

;; (elsa-type-get-args :: (function (mixed) (list mixed)))
(cl-defgeneric elsa-type-get-args (_thing)
  "Get argument types of THING."
  nil)

;; (elsa-type-get-return :: (and (function (nil) nil) (function ((class elsa-type)) (class elsa-type))))
(cl-defgeneric elsa-type-get-return (_this)
  "Get return type of THIS type."
  nil)

(cl-defmethod elsa-type-get-return ((this elsa-type))
  this)

(cl-defmethod elsa-type-accept ((this elsa-type) (other elsa-type) &optional explainer)
  "Test if THIS type accepts OTHER.

Accepting in this context means that OTHER can be assigned to
THIS."
  (elsa-with-explainer explainer
    (elsa--fmt-explain-type-0-does-not-accept-type-1
     (elsa-tostring this) (elsa-tostring other))
    (cond
     ((elsa-instance-of other this))
     ((elsa-const-type-p other)
      (elsa-type-accept this (oref other type) explainer))
     ((and (elsa-type-list-p this)
           (elsa-type-nil-p other)))
     ((elsa-type-composite-p other)
      (elsa-type-is-accepted-by other this explainer)))))

;; (elsa-type-composite-p :: (function (mixed) bool))
(cl-defgeneric elsa-type-composite-p (_this)
  "Determine if the type is a composite type.

Composite types have to be wrapped in parens when passed as
arguments to other constructors."
  nil)

(cl-defmethod elsa-type-composite-p ((_this elsa-composite-type)) t)

;; (elsa-type-callable-p :: (function ((class elsa-type)) bool))
(cl-defgeneric elsa-type-callable-p (_this)
  "Check if the type is callable.

A callable type can be called either directly as a list form or
with `funcall' or `apply' (or with other similar functions)."
  nil)

;; (elsa-function-type-nth-arg :: (function (mixed (or int symbol)) mixed))
(cl-defgeneric elsa-function-type-nth-arg (_this _n)
  "Return type of Nth argument or named argument.

For non-callable functions, return nil.

If N is more or equal to the arity of the function and the last
argument is variadic, return that type, otherwise return nil.

If N is not a number, it is assumed to be a slot name which we
look up in the last keys argument type."
  nil)

(defclass elsa-type-empty (elsa-type elsa-simple-type eieio-singleton) ()
  :documentation "Empty type.  Has no domain.

This is accepted by any type and does not accept any type except
empty.")

(cl-defmethod elsa-type-describe ((_this elsa-type-empty))
  "empty")

(cl-defmethod elsa-type-accept ((this elsa-type-empty) (other elsa-type) &optional explainer)
  (if (elsa-type-composite-p other)
      (elsa-type-is-accepted-by other this explainer)
    (elsa-with-explainer explainer
      (elsa--fmt-explain-type-0-only-accepts-type-1-was-2
       "empty" "empty" (elsa-tostring other))
      (elsa-type-empty-p other))))

(cl-defmethod elsa-type-accept ((_this elsa-type) (_this2 elsa-type-empty) &optional _explainer)
  t)

(defun elsa-type-sum-all (types)
  "Sum of all the TYPES.

This will sum the empty type with the first type, then the result
of this with the second type and so on until all types are
summed.

Type sum is similar to set union in that it is commutative and
associative."
  (-reduce-from #'elsa-type-sum (elsa-type-empty) types))

(defun elsa-type-intersect-all (types)
  "Intersect of all the TYPES.

This will intersect the mixed type with the first type, then the
result of this with the second type and so on until all types are
intersected.

Type intersect is similar to set intersection in that it is
commutative and associative."
  (-reduce-from #'elsa-type-intersect (elsa-type-mixed) types))

(defclass elsa-intersection-type (elsa-type elsa-composite-type)
  ((types :initform nil :initarg :types))
  :documentation "Intersection type.

This type is an intersection of multiple types.

It can accept any type that is all the types of the intersection.

It is accepted by any of the intersected types because it is all
of them.")

(cl-defmethod clone ((this elsa-intersection-type))
  "Make a deep copy of a intersection type."
  (let ((types (-map 'clone (oref this types)))
        (new (cl-call-next-method)))
    (oset new types types)
    new))

(cl-defmethod elsa-type-accept ((this elsa-intersection-type) (other elsa-type) &optional explainer)
  (elsa-with-explainer explainer
    (elsa--fmt-explain-type-0-does-not-accept-type-1
     (elsa-tostring this) (elsa-tostring other))
    (-all? (lambda (type) (elsa-type-accept type other explainer)) (oref this types))))

(cl-defmethod elsa-type-is-accepted-by ((this elsa-intersection-type) other &optional explainer)
  (-any? (lambda (type) (elsa-type-accept other type explainer)) (oref this types)))

(cl-defmethod elsa-type-describe ((this elsa-intersection-type))
  (format "(and %s)" (mapconcat 'elsa-type-describe (oref this types) " ")))

(cl-defmethod elsa-type-callable-p ((this elsa-intersection-type))
  "An intersection type is callable if at least one intersected type is callable."
  (let ((types (oref this types)))
    (cond
     ((= 0 (length types)) nil)
     (t (-any? #'elsa-type-callable-p types)))))

(cl-defmethod elsa-type-get-return ((this elsa-intersection-type))
  "Return type of an intersection is the sum of return types of intersected types.

In case of primitive types the return type is the type it self.
In case of functions, it is the return type of the function."
  (-reduce 'elsa-type-sum (-map 'elsa-type-get-return (oref this types))))

(cl-defmethod elsa-function-type-nth-arg ((this elsa-intersection-type) n)
  (let ((types (oref this types)))
    (cond
     ((= 0 (length types)) nil)
     (t (-when-let (summands (--keep (elsa-function-type-nth-arg it n) types))
          (elsa-type-sum-all summands))))))

(defclass elsa-sum-type (elsa-type elsa-composite-type)
  ((types :type list
          :initarg :types
          :initform nil))
  :documentation "Sum type.

This type is a combination of other types.

It can accept any type that is accepted by at least one of its
summands.

It is accepted by any type that is all of the summed types
because the actual type can be any of them.")

(cl-defmethod elsa-type-describe ((this elsa-sum-type))
  (format "(or %s)" (mapconcat 'elsa-type-describe (oref this types) " ")))

(cl-defmethod clone ((this elsa-sum-type))
  "Make a deep copy of a sum type."
  (let ((types (-map 'clone (oref this types)))
        (new (cl-call-next-method)))
    (oset new types types)
    new))

(cl-defmethod elsa-type-accept ((this elsa-sum-type) (other elsa-type) &optional explainer)
  (cond
   ((elsa-type-composite-p other)
    (elsa-type-is-accepted-by other this explainer))
   ((and (null (oref this types))
         (elsa-type-empty-p other)))
   (t
    (elsa-with-explainer explainer
      (elsa--fmt-explain-type-0-does-not-accept-type-1
       (elsa-tostring this) (elsa-tostring other))
      (-any? (lambda (ot) (elsa-type-accept ot other explainer)) (oref this types))))))

(cl-defmethod elsa-type-is-accepted-by ((this elsa-sum-type) other &optional explainer)
  (-all? (lambda (type) (elsa-type-accept other type explainer)) (oref this types)))

(cl-defmethod elsa-type-callable-p ((this elsa-sum-type))
  "A sum type is callable only if all the summands are callable."
  (let ((types (oref this types)))
    (cond
     ((= 0 (length types)) nil)
     (t (-all? #'elsa-type-callable-p types)))))

(cl-defmethod elsa-function-type-nth-arg ((this elsa-sum-type) n)
  (let ((types (oref this types)))
    (cond
     ((= 0 (length types)) nil)
     (t (-when-let (summands (--keep (elsa-function-type-nth-arg it n) types))
          (elsa-type-intersect-all summands))))))

;; TODO: is this true?  It seems that return type of intersection of
;; functions is also a sum of their return types... and this kind of
;; breaks the expected symmetry.
(cl-defmethod elsa-type-get-return ((this elsa-sum-type))
  "Return type of a sum is the sum of return types of its types.

In case of primitive types the return type is the type it self.
In case of functions, it is the return type of the function."
  (-reduce 'elsa-type-sum (-map 'elsa-type-get-return (oref this types))))

(defclass elsa-diff-type (elsa-type elsa-composite-type)
  ((positive :initform (progn (elsa-type-mixed)) :initarg :positive)
   (negative :initform (progn (elsa-type-empty)) :initarg :negative))
  :documentation "Diff type.

This type is a combination of positive and negative types.  It
can accept any type that is accepted by at least one positive
type and none of the negative types.")

(cl-defmethod clone ((this elsa-diff-type))
  "Make a deep copy of a diff type."
  (let ((positive (clone (oref this positive)))
        (negative (clone (oref this negative)))
        (new (elsa-diff-type)))
    (oset new positive positive)
    (oset new negative negative)
    new))

(cl-defmethod elsa-type-accept ((this elsa-diff-type) (other elsa-type) &optional explainer)
  (elsa-with-explainer explainer
    (elsa--fmt-explain-type-0-does-not-accept-type-1
     (elsa-tostring this) (elsa-tostring other))
    (and (elsa-type-accept (oref this positive) other explainer)
         (elsa-with-explainer explainer
           ("Intersection of subtracted type `%s' and tested type `%s' is not empty"
            (elsa-tostring (oref this negative)) (elsa-tostring other))
           (elsa-type-empty-p
            (elsa-type-intersect (oref this negative) other))))))

(cl-defmethod elsa-type-is-accepted-by ((this elsa-diff-type) other &optional explainer)
  (elsa-type-accept other (oref this positive) explainer))

(cl-defmethod elsa-type-describe ((this elsa-diff-type))
  (if (oref this negative)
      (format "(diff %s %s)"
              (elsa-type-describe (oref this positive))
              (elsa-type-describe (oref this negative)))
    (elsa-type-describe (oref this positive))))

(defclass elsa-type-symbol (elsa-type elsa-simple-type eieio-singleton) ()
  :documentation "Quoted symbol")

(cl-defmethod elsa-type-describe ((_this elsa-type-symbol))
  "symbol")

(defclass elsa-type-bool (elsa-type elsa-type-symbol)
  ((predicates
    :initarg :predicates
    :documentation "Type to which the function's argument will be narrowed.

Any time a function is called with some variable, Elsa will narrow
that variable to this specific type if the original type is
compatible.")))

(defclass elsa-type-t (elsa-type-bool) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-t))
  "t")

(defclass elsa-type-nil (elsa-type-bool) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-nil))
  "nil")

(cl-defmethod elsa-type-describe ((this elsa-type-bool))
  (if (slot-boundp this 'predicates)
      (format "(is %s)" (elsa-type-describe (oref this predicates)))
    "bool"))

(defun elsa-type-is-type-predicate-p (this)
  "Return non-nil if THIS is a type predicate.

A type predicate is always of type bool and additionally narrows
the type of its argument to the predicated type."
  (and (elsa-type-bool-p this)
       (slot-boundp this 'predicates)))

(defclass elsa-type-sequence (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-sequence))
  "sequence")

(cl-defmethod elsa-type-get-item-type ((_this elsa-type))
  "Get the type of items of a sequence type."
  nil)

(defclass elsa-type-string (elsa-type-sequence elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-get-item-type ((_this elsa-type-string))
  "Get the type of items of a sequence type."
  (elsa-type-int))

(defclass elsa-type-short-string (elsa-type-string) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-string))
  "string")

(defclass elsa-type-buffer (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-buffer))
  "buffer")

(defclass elsa-type-frame (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-frame))
  "frame")

(defclass elsa-type-number (elsa-type elsa-simple-type eieio-singleton)
  ()
  :documentation "Type of any number.

Numbers can be integers or floats.  Even though mathematically
integers are a subset of floats, Elsa considers them two separate
non-overlapping sets.

The type number is then a set-theoretic union of these.

For convenience, float type accepts int type because that is how Emacs
operates.  This breaks the assumption that acceptance corresponds to
the set-theoretic inclusion of values, because int values are not part
of the float type domain.

The accept rules are as follows:

       number  float  int
number   ✓       ✓     ✓
float    x       ✓     ✓
int      x       x     ✓

The type sum (row + column) resolution is as follows:

         number   float    int
number   number   number  number
float    number    float  number
int      number   number   int


The type intersection (and row column) resolution is as follows:

         number   float    int
number   number   float    int
float    float    float   empty
int       int     empty    int


The type diff (row - column) resolution is as follows:

        number  float  int
number  empty    int  float
float   empty   empty float
int     empty    int  empty

")

(cl-defmethod elsa-type-describe ((_this elsa-type-number))
  "number")

(defclass elsa-type-float (elsa-type-number)
  ()
  :documentation "Floating point number.

See `elsa-type-number' for explanation of relationship of types int,
float and number.")

(cl-defmethod elsa-type-describe ((_this elsa-type-float))
  "float")

(defclass elsa-type-int (elsa-type-float)
  ()
  :documentation "Integer number.

See `elsa-type-number' for explanation of relationship of types int,
float and number.")

(cl-defmethod elsa-type-describe ((_this elsa-type-int))
  "int")

(defclass elsa-type-marker (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-marker))
  "marker")

(defclass elsa-type-keyword (elsa-type-symbol) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-keyword))
  "keyword")

(defclass elsa-type-cons (elsa-type)
  ((car-type :type elsa-type :initarg :car-type
             :initform (progn (elsa-type-mixed)))
   (cdr-type :type elsa-type :initarg :cdr-type
             :initform (progn (elsa-type-mixed)))))

(cl-defmethod clone ((this elsa-type-cons))
  "Make a deep copy of a cons type."
  (let ((car-type (clone (oref this car-type)))
        (cdr-type (clone (oref this cdr-type)))
        (new (cl-call-next-method)))
    (oset new car-type car-type)
    (oset new cdr-type cdr-type)
    new))

(cl-defmethod elsa-type-accept ((this elsa-type-cons) (other elsa-type-cons) &optional explainer)
  "A cons type accepts another cons type covariantly.

That means that iff both arguments of this are supertypes of
other, then this is a supertype of other."
  (elsa-with-explainer explainer
    (elsa--fmt-explain-type-0-does-not-accept-type-1
     (elsa-tostring this) (elsa-tostring other))
    (and (elsa-with-explainer explainer
           ("Car of `%s' does not accept `%s'"
            (elsa-tostring this) (elsa-tostring other))
           (elsa-type-accept (oref this car-type) (oref other car-type) explainer))
         (elsa-with-explainer explainer
           ("Cdr of `%s' does not accept `%s'"
            (elsa-tostring this) (elsa-tostring other))
           (elsa-type-accept (oref this cdr-type) (oref other cdr-type) explainer)))))

(cl-defmethod elsa-type-is-accepted-by ((_this elsa-type-cons) (_other elsa-type-empty) &optional _explainer)
  "A cons type is not accepted by empty."
  nil)

(cl-defmethod elsa-type-describe ((this elsa-type-cons))
  (format "(cons %s %s)"
          (elsa-type-describe (oref this car-type))
          (elsa-type-describe (oref this cdr-type))))

(defclass elsa-type-tuple (elsa-type)
  ((types :type list :initarg :types)))

(cl-defmethod clone ((this elsa-type-tuple))
  "Make a deep copy of a cons type."
  (let ((types (-map 'clone (oref this types)))
        (new (cl-call-next-method)))
    (oset new types types)
    new))

(cl-defmethod elsa-type-accept ((this elsa-type-tuple) (other elsa-type-tuple) &optional explainer)
  (elsa-with-explainer explainer
    (elsa--fmt-explain-type-0-does-not-accept-type-1
     (elsa-tostring this) (elsa-tostring other))
    (catch 'ok
      ;; types are accepted co-variantly
      (let ((this-types (oref this types))
            (other-types (oref other types))
            (i 0))
        (unless (= (length this-types) (length other-types))
          (elsa-explain explainer
            "Different number of elements in tuple, this has %d, other has %d"
            (length this-types) (length other-types))
          (throw 'ok nil))
        (cl-mapc
         (lambda (this-type other-type)
           (cl-incf i)
           (unless (elsa-with-explainer explainer
                     ("Elements %d. is not assignable" i)
                     (elsa-type-accept this-type other-type explainer))
             (throw 'ok nil)))
         this-types other-types))
      t)))

(cl-defmethod elsa-type-describe ((this elsa-type-tuple))
  (format "(%s)" (mapconcat 'elsa-type-describe (oref this types) " ")))

(defclass elsa-type-list (elsa-type-cons elsa-type-sequence)
  ((item-type :type elsa-type
              :initarg :item-type
              :initform (progn (elsa-type-mixed)))))

(cl-defmethod clone ((this elsa-type-list))
  "Make a deep copy of a list type."
  (let ((item-type (clone (oref this item-type)))
        (new (cl-call-next-method)))
    (oset new item-type item-type)
    new))

(cl-defmethod elsa-type-accept ((this elsa-type-list) (other elsa-type) &optional explainer)
  "A list type accepts another list type covariantly.

That means that iff the item type of this is supertype of other,
then this is a supertype of other.

Note that a list here is a homogeneous list, meaning a list of
unknown length where elements are all of the same type.

For a list of fixed length with heterogeneous types, see
`elsa-type-tuple'."
  (elsa-with-explainer explainer
    (elsa--fmt-explain-type-0-does-not-accept-type-1
     (elsa-tostring this) (elsa-tostring other))
    (cond
     ((elsa-type-nil-p other))
     ((elsa-type-tuple-p other)
      ;; we can accept a tuple if all of its types are accepted by this
      (elsa-with-explainer explainer
        ("Some element of tuple does not match this list's item type")
        (-all?
         (lambda (ot) (elsa-type-accept (oref this item-type) ot explainer))
         (oref other types))))
     ((elsa-type-plist-p other)
      (let ((plist-type (elsa-type-empty)))
        (maphash
         (lambda (_key slot)
           (setq plist-type
                 (-> plist-type
                     (elsa-type-sum (elsa--make-type `(const ,(oref slot name))))
                     (elsa-type-sum (oref slot type)))))
         (oref other slots))
        (elsa-with-explainer explainer
          (elsa--fmt-explain-sequence-item-types-of-these-0-do-not-match "lists")
          (elsa-type-accept (oref this item-type) plist-type explainer))))
     ((elsa-type-list-p other)
      (elsa-with-explainer explainer
        (elsa--fmt-explain-sequence-item-types-of-these-0-do-not-match "lists")
        (elsa-type-accept (oref this item-type) (oref other item-type) explainer)))
     ((elsa-type-composite-p other)
      (elsa-type-is-accepted-by other this explainer)))))

(cl-defmethod elsa-type-describe ((this elsa-type-list))
  (format "(list %s)" (elsa-type-describe (oref this item-type))))

(cl-defmethod elsa-type-get-item-type ((this elsa-type-list))
  "Get the type of items of a sequence type."
  (oref this item-type))

(defclass elsa-interface (elsa-type elsa-type--structured)
  ((name :type symbol :initarg :name))
  :documentation "Abstract description of an object-like type.

Each interface is uniquely determined by its name.

Interfaces hold slots.  Each slot must have a name and a type and
other optional attributes (see `elsa-structure-slot').

Interfaces can not be instantiated by themselves, but can be
*implemented* by other structured types.

For example, you can construct a plist by either supplying the list of
names and types or by only supplying the interface name, at which
point the plist will gain all the slots defined by the interface.

The same interface can be used to define plists or classes or any
other structured type.

Structured types all inherit from `elsa-type--structured'.")

(cl-defmethod elsa-type-describe ((this elsa-interface))
  (let* ((slots (oref this slots))
         (keys (-sort #'string< (hash-table-keys slots))))
    (format "(interface %s %s)"
            (oref this name)
            (mapconcat
             (lambda (key)
               (let ((slot (gethash key slots)))
                 (format "%s %s"
                         (symbol-name (oref slot name))
                         (elsa-tostring (oref slot type)))))
             keys
             " "))))

(defclass elsa-type-plist (elsa-type-list elsa-type--structured)
  ()
  :documentation "Ad-hoc interface-like structure.

The plist data structure is actually a simple list with alternating
keys and values.

One key and value pair is called a slot.

An example of plist is (:one 1 :two 2).  Elsa currently supports
symbols as keys.")

(defun elsa-type-make-plist-hashtable (pairs)
  "Make hashtable of slots from PAIRS of keywords and types.

Each item of PAIRS is a list (KEYWORD ELSA-TYPE)"
  (let ((hashtable (make-hash-table)))
    (dolist (pair pairs)
      (let ((keyword (car pair))
            (type (cadr pair)))
        (puthash keyword (elsa-structure-slot :name keyword :type type) hashtable)))
    hashtable))

(defclass elsa-type-keys (elsa-type-plist) ()
  "This is a type of keyword arguments and available slots.

It is the same as plist except it is only used to denote that
this plist type is supposed to be expanded into key-value pairs
rather than consumed as a single value in the argument list.")

(cl-defmethod elsa-type-accept ((this elsa-type-plist) (other elsa-type-plist) &optional explainer)
  (elsa-with-explainer explainer
    (elsa--fmt-explain-type-0-does-not-accept-type-1
     (elsa-tostring this) (elsa-tostring other))
    (elsa-type--check-slots this other #'elsa-type-accept explainer)))

(cl-defmethod elsa-type-describe ((this elsa-type-plist))
  (let* ((slots (oref this slots))
         (keys (-sort #'string< (hash-table-keys slots)))
         (extends (-sort #'string< (oref this extends))))
    (format "(%s %s%s)"
            (if (elsa-type-plist-p this) "plist" "keys")
            (mapconcat
             (lambda (key)
               (let ((slot (gethash key slots)))
                 (format "%s %s"
                         (symbol-name (oref slot name))
                         (elsa-tostring (oref slot type)))))
             keys
             " ")
            (if extends
                (format " &extends %s"
                        (mapconcat #'symbol-name extends " "))
                ""))))

(defclass elsa-type-vector (elsa-type-sequence)
  ((item-type :type elsa-type
              :initarg :item-type
              :initform (progn (elsa-type-mixed)))))

(cl-defmethod clone ((this elsa-type-vector))
  "Make a deep copy of a vector type."
  (let ((item-type (clone (oref this item-type)))
        (new (cl-call-next-method)))
    (oset new item-type item-type)
    new))

(cl-defmethod elsa-type-accept ((this elsa-type-vector) (other elsa-type-vector) &optional explainer)
  "A vector type accepts another vector type covariantly.

That means that iff the item type of this is supertype of other,
then this is a supertype of other."
  (elsa-with-explainer explainer
    (elsa--fmt-explain-sequence-item-types-of-these-0-do-not-match "vectors")
    (elsa-type-accept (oref this item-type) (oref other item-type) explainer)))

(cl-defmethod elsa-type-describe ((this elsa-type-vector))
  (format "(vector %s)" (elsa-type-describe (oref this item-type))))

(cl-defmethod elsa-type-get-item-type ((this elsa-type-vector))
  "Get the type of items of a sequence type."
  (oref this item-type))

(defclass elsa-variadic-type (elsa-type-list) nil)

(cl-defmethod elsa-type-describe ((this elsa-variadic-type))
  (format "&rest %s" (elsa-type-describe (oref this item-type))))

(defclass elsa-function-type (elsa-type)
  ((args :type list :initarg :args)
   (return :type elsa-type :initarg :return)))

(cl-defmethod clone ((this elsa-function-type))
  "Make a deep copy of a function type."
  (let ((args (-map 'clone (oref this args)))
        (return (clone (oref this return)))
        (new (cl-call-next-method)))
    (oset new args args)
    (oset new return return)
    new))

(cl-defmethod elsa-type-describe ((this elsa-function-type))
  (format "(function (%s) %s)"
          (mapconcat 'elsa-type-describe (oref this args) " ")
          (elsa-type-describe (oref this return))))

(cl-defmethod elsa-type-accept ((this elsa-function-type) (other elsa-type) &optional explainer)
  (if (elsa-type-composite-p other)
      (elsa-type-is-accepted-by other this explainer)
    (if (elsa-function-type-p other)
        ;; Argument types must be contra-variant, return types must be
        ;; co-variant.
        (elsa-with-explainer explainer
          (elsa--fmt-explain-type-0-does-not-accept-type-1
           (elsa-tostring this) (elsa-tostring other))
          (catch 'ok
            (let ((this-args (oref this args))
                  (other-args (oref other args))
                  (i 0))
              (unless (= (length this-args) (length other-args))
                (elsa-explain explainer "Different number of arguments")
                (throw 'ok nil))
              (cl-mapc
               (lambda (this-arg other-arg)
                 (cl-incf i)
                 (unless (elsa-with-explainer explainer
                           ("Argument %d not compatible" i)
                           (elsa-type-accept other-arg this-arg explainer))
                   (throw 'ok nil)))
               this-args other-args)
              (unless (elsa-with-explainer explainer
                        ("Return types not compatible")
                        (elsa-type-accept
                         (elsa-type-get-return this)
                         (elsa-type-get-return other)
                         explainer))
                (throw 'ok nil)))
            t))
      (elsa-with-explainer explainer
        ("Function type `%s' can only accept function types, was `%s'"
         (elsa-tostring this) (elsa-tostring other))
        nil))))

(cl-defmethod elsa-type-callable-p ((_this elsa-function-type)) t)

(cl-defmethod elsa-function-type-nth-arg ((this elsa-function-type) n)
  (let* ((args (oref this args)))
    (cond
     ((symbolp n)
      (let ((last-type (-last-item args)))
        (when (elsa-type-keys-p last-type)
          (when-let ((slot (elsa-get-slot last-type n)))
            (elsa-get-type slot)))))
     ((<= (1- (length args)) n)
      (let ((last-type (-last-item args)))
        (if (elsa-variadic-type-p last-type)
            (oref last-type item-type)
          last-type)))
     (t (nth n args)))))

(cl-defmethod elsa-type-get-args ((this elsa-function-type))
  "Get argument types of THIS function type."
  (oref this args))

(cl-defmethod elsa-type-get-return ((this elsa-function-type))
  (oref this return))

(defclass elsa-generic-type (elsa-type)
  ((label :type symbol :initarg :label)))

(cl-defmethod clone ((this elsa-generic-type))
  "Make a deep copy of a generic type."
  (let ((label (oref this label))
        (new (cl-call-next-method)))
    (oset new label label)
    new))

(cl-defmethod elsa-type-describe ((this elsa-generic-type))
  (symbol-name (oref this label)))

;; One-dimensional sparse arrays indexed by characters
(defclass elsa-type-char-table (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-char-table))
  "char-table")

;; One-dimensional arrays of t or nil.
(defclass elsa-type-bool-vector (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-bool-vector))
  "bool-vector")

;; Super-fast lookup tables
;; TODO: add key and value types
(defclass elsa-type-hash-table (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-hash-table))
  "hash-table")

;; Compound objects with programmer-defined types
(defclass elsa-type-record (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-record))
  "record")

;; Buffers are displayed in windows
(defclass elsa-type-window (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-window))
  "window")

;; A terminal device displays frames
(defclass elsa-type-terminal (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-terminal))
  "terminal")

;; Recording the way a frame is subdivided
(defclass elsa-type-window-configuration (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-window-configuration))
  "window-configuration")

;; Recording the status of all frames
(defclass elsa-type-frame-configuration (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-frame-configuration))
  "frame-configuration")

;; A subprocess of Emacs running on the underlying OS
(defclass elsa-type-process (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-process))
  "process")

;; A thread of Emacs Lisp execution
(defclass elsa-type-thread (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-thread))
  "thread")

;; An exclusive lock for thread synchronization
(defclass elsa-type-mutex (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-mutex))
  "mutex")

;; Condition variable for thread synchronization
(defclass elsa-type-condition-variable (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-condition-variable))
  "condition-variable")

;; Receive or send characters
(defclass elsa-type-stream (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-stream))
  "stream")

;; What function a keystroke invokes
(defclass elsa-type-keymap (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-keymap))
  "keymap")

;; How an overlay is represented
(defclass elsa-type-overlay (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-overlay))
  "overlay")

;; Fonts for displaying text
(defclass elsa-type-font (elsa-type elsa-simple-type eieio-singleton) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-font))
  "font")

(defclass elsa-const-type (elsa-type)
  ((type :type elsa-type :initarg :type)
   (value :initarg :value))
  :documentation "A type of a concrete value.

When we use a constant literal, it has a const type.  Const wraps
a real type, such as int or string, but can only take one
predefined value.")

(cl-defmethod clone ((this elsa-const-type))
  "Make a deep copy of a intersection type."
  (let ((type (clone (oref this type)))
        (new (cl-call-next-method)))
    (oset new type type)
    new))

(cl-defmethod elsa-type-describe ((this elsa-const-type))
  (format "(const %S)" (oref this value)))

(cl-defmethod elsa-type-accept ((this elsa-const-type) (other elsa-type) &optional explainer)
  "The const type is different from readonly in that a readonly
type can never be assigned to but a const type is only a
narrowing of a type to a concrete value from the type's domain.

Const type only accepts other const type whose value is `equal'
to this value."
  (elsa-with-explainer explainer
    (elsa--fmt-explain-type-0-does-not-accept-type-1
     (elsa-tostring this) (elsa-tostring other))
    (and (elsa-with-explainer explainer
           (elsa--fmt-explain-0-is-not-a-1
            (elsa-tostring other) "const type")
           (elsa-const-type-p other))
         (elsa-type-accept (oref this type) (oref other type) explainer)
         (elsa-with-explainer explainer
           ("Values `%s' and `%s' are not equal"
            (oref this value) (oref other value))
           (equal (oref this value) (oref other value))))))

;; Readonly type for defconst
(defclass elsa-readonly-type (elsa-type elsa-composite-type)
  ((type :type elsa-type :initarg :type))
  :documentation "Read-only (or constant) type.

It wraps any other type and makes the form or variable
unassignable.")

(cl-defmethod elsa-type-accept ((this elsa-readonly-type) (other elsa-type) &optional explainer)
  "Check if this readonly type accept other type.

The acceptance does not depend on the readonly status.  Here we
simply work with the wrapped types.  The readonly type wrapper is
only a flag which is used during assignability checks (such as
`setq'), not as a type check!"
  (if (elsa-type-composite-p other)
      (elsa-type-is-accepted-by other (oref this type) explainer)
    (elsa-type-accept (oref this type) other explainer)))

(cl-defmethod elsa-type-is-accepted-by ((this elsa-readonly-type) other &optional explainer)
  (elsa-type-accept other (oref this type) explainer))

(cl-defmethod elsa-type-describe ((this elsa-readonly-type))
  (format "(readonly %s)" (elsa-type-describe (oref this type))))

(cl-defmethod elsa-type-accept ((_this null) _other &optional _explainer)
  "This method catches the impossible situation when we are
trying to analyse 'nil, which is not a valid type or form."
  (message "An error happened trying to analyse nil")
  (backtrace))

(defclass elsa-struct-type (elsa-type)
  ((name
    :type (or symbol nil)
    :initarg :name))
  :documentation "Type representing a `cl-defstruct'.")

(cl-defmethod elsa-get-slots ((this elsa-struct-type))
  (when-let ((class (get (oref this name) 'elsa-defstruct)))
    (elsa-get-slots class)))

(cl-defmethod elsa-get-slot ((this elsa-struct-type) (name symbol))
  (when-let ((class (get (oref this name) 'elsa-defstruct)))
    (elsa-get-slot class name)))

(cl-defmethod elsa-type-accept ((this elsa-struct-type) (other elsa-type) &optional explainer)
  (cond
   ((elsa-type-composite-p other)
    (elsa-type-is-accepted-by other this explainer))
   ((elsa-struct-type-p other)
    (let ((this-struct (get (oref this name) 'elsa-defstruct))
          (other-struct (get (oref other name) 'elsa-defstruct))
          ;; (this-interface (get (oref this name) 'elsa-interface))
          ;; (other-interface (get (oref other name) 'elsa-interface))
          )
      (cond
       ((not this-struct))
       ((and this-struct other-struct)
        (not (null (assq (oref this-struct name) (oref other-struct parents)))))
       ;; ((and this-struct other-interface)
       ;;  (elsa-type--check-slots this-struct other-interface #'elsa-type-assignable-p explainer))
       ;; ((and this-interface other-struct)
       ;;  (elsa-type--check-slots this-interface other-struct #'elsa-type-assignable-p explainer))
       ;; ((and this-interface other-interface)
       ;;  (elsa-type--check-slots this-interface other-interface #'elsa-type-assignable-p explainer))
       )))))

(cl-defmethod elsa-type-describe ((this elsa-struct-type))
  (format "(struct %s)" (oref this name)))

(defclass elsa-class-type (elsa-type)
  ((name
    :type (or symbol nil)
    :initarg :name))
  :documentation "Type representing a `defclass'.")

(cl-defmethod elsa-type-describe ((this elsa-class-type))
  (format "(class %s)" (oref this name)))

(cl-defmethod elsa-get-slots ((this elsa-class-type))
  (when-let ((class (get (oref this name) 'elsa-defclass)))
    (elsa-get-slots class)))

(cl-defmethod elsa-get-slot ((this elsa-class-type) (name symbol))
  (when-let ((class (get (oref this name) 'elsa-defclass)))
    (elsa-get-slot class name)))

(cl-defmethod elsa-type-accept ((this elsa-class-type) (other elsa-type) &optional explainer)
  (cond
   ((elsa-type-composite-p other)
    (elsa-type-is-accepted-by other this explainer))
   ;; if we accept interface directly, it should not declare
   ;; properties but only methods
   ;; ((elsa-interface-p other)
   ;;  (let ((this-class (get (oref this name) 'elsa-defclass))
   ;;        (this-interface (get (oref this name) 'elsa-interface)))
   ;;    (cond
   ;;     (this-class
   ;;      (elsa-type--check-slots this-class other #'elsa-type-assignable-p explainer))
   ;;     (this-interface
   ;;      (elsa-type--check-slots this-interface other #'elsa-type-assignable-p explainer)))))
   ((elsa-class-type-p other)
    ;; class type (class nil) accepts any class
    (if (not (oref this name)) t
      (let ((this-class (get (oref this name) 'elsa-defclass))
            (other-class (get (oref other name) 'elsa-defclass))
            (this-interface (get (oref this name) 'elsa-interface))
            (other-interface (get (oref other name) 'elsa-interface)))
        (cond
         ((elsa-with-explainer explainer
            ("The class or interface %s is not defined" (oref this name))
            (and (not this-class) (not this-interface))))
         ((elsa-with-explainer explainer
            ("The class or interface %s is not defined" (oref other name))
            (and (not other-class) (not other-interface))))
         ((and this-class other-class)
          (elsa-with-explainer explainer
            ("Class %s is not subtype of %s" (oref other name) (oref this name))
            (not (null (assq (oref this-class name) (oref other-class parents))))))
         ((and this-class other-interface)
          (elsa-type--check-slots this-class other-interface #'elsa-type-assignable-p explainer))
         ((and this-interface other-class)
          (elsa-type--check-slots this-interface other-class #'elsa-type-assignable-p explainer))
         ((and this-interface other-interface)
          (elsa-type--check-slots this-interface other-interface #'elsa-type-assignable-p explainer))))))))

(defclass elsa-type--cl-ref (elsa-type)
  ((name
    :type (or symbol nil)
    :initarg :name))
  :documentation "Reference to a cl-typep compatible type.

This is used instead of `elsa-class-type' or `elsa-struct-type' if the
referenced type was not yet defined and we don't know what exactly the
symbol represents.  During checking, we will try to forward to
`elsa-type-accept' of appropriate type.")

(defun elsa-type--resolve-cl-ref (cl-ref)
  (cond
   ((get (oref cl-ref name) 'elsa-defclass)
    (elsa-class-type :name (oref cl-ref name)))
   ((get (oref cl-ref name) 'elsa-defstruct)
    (elsa-struct-type :name (oref cl-ref name)))
   ((elsa-type-unbound))))

(cl-defmethod elsa-type-describe ((this elsa-type--cl-ref))
  (format "(cl-ref %s)" (oref this name)))

(cl-defmethod elsa-type-accept ((this elsa-type--cl-ref) (other elsa-type) &optional explainer)
  (elsa-type-accept (elsa-type--resolve-cl-ref this) other explainer))

(cl-defmethod elsa-type-accept ((this elsa-class-type) (other elsa-type--cl-ref) &optional explainer)
  (elsa-type-accept this (elsa-type--resolve-cl-ref other) explainer))

(cl-defmethod elsa-type-accept ((this elsa-struct-type) (other elsa-type--cl-ref) &optional explainer)
  (elsa-type-accept this (elsa-type--resolve-cl-ref other) explainer))

(provide 'elsa-types)
;;; elsa-types.el ends here
