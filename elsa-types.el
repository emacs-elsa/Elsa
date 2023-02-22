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
(require 'cl-macs)

(require 'trinary)
(require 'dash)

(require 'elsa-methods)

(defclass elsa-type nil () :abstract t)

;; (elsa-type-describe :: (function (mixed) string))
(cl-defgeneric elsa-type-describe (type)
  "Return a string representation of TYPE."
  (format "%s" type))

(cl-defmethod elsa-tostring ((this elsa-type))
  (elsa-type-describe this))

(cl-defmethod cl-print-object ((this elsa-type) stream)
  (princ (elsa-tostring this) stream))

(defclass elsa-composite-type nil ()
  :abstract t
  :documentation "Composite type holding other types.

This type is a wrapper around one or more other types and
modifies their behaviour.")

(cl-defgeneric elsa-type-accept (this other)
  "Check if THIS accepts OTHER.

This means OTHER is assignable to THIS.

This method only performs soundness analysis.  To account for the
special handling of `elsa-type-mixed', use
`elsa-type-assignable-p'.")

(cl-defgeneric elsa-type-could-accept ((this elsa-type) (other elsa-type))
  "Check if THIS could accept OTHER.

Return a trinary logical value:

- trinary-true if THIS always accepts OTHER,
- trinary-maybe if THIS can accept OTHER sometimes and can reject
  sometimes,
- trinary-false if THIS never accepts OTHER."
  (cond
   ((elsa-type-accept this other)
    (trinary-true))
   ((not (elsa-type-empty-p (elsa-type-intersect this other)))
    (trinary-maybe))
   (t (trinary-false))))

(cl-defgeneric elsa-type-is-accepted-by (this other)
  "Check if THIS is accepted by OTHER.

This means THIS is assignable to OTHER.

This method is used to unpack composite types and perform a
\"double dispatch\" when testing one composite type accepting
another, because there is a many-to-many relationship."
  (elsa-type-accept other this))

;; Mixed type is special in that it is always created nullable.  Mixed
;; can also serve as bool type in Emacs Lisp.
(defclass elsa-type-mixed (elsa-type) ()
  :documentation "Type of anything.

Mixed is a special type in a way that it serves as an opt-out
from type analysis.

Mixed accepts any other type except unbound.  This is
type-theoretically sound because mixed is the union of all types.

However, mixed is also accepted by any other type.  This is
unsafe and works as an implicit type-casting.

Typically, if some function returns mixed, such as reading a
property from a symbol with `get', we can wrap this call in a
function and provide a more narrow type signature for the return
type provided we know that the property is always of a certain
type.

  ;; (set-property :: (function (symbol int) int))
  (defun set-property (name value)
    (set name value))

  ;; (get-property :: (function (symbol) int))
  (defun get-property (name)
    ;; Even though `get' returns mixed, we know that if we only set it
    ;; with `set-property' it will always be int and so we can provide
    ;; more specific return type.
    (get name value))")

(cl-defmethod elsa-type-describe ((_this elsa-type-mixed))
  "mixed")

(cl-defmethod elsa-type-accept ((_this elsa-type-mixed) other)
  (unless (cl-typep other 'elsa-type)
    (cl-call-next-method)
    ;(error "Other must be subclass of `elsa-type'")
    )
  (not (elsa-type-unbound-p other)))

(defun elsa-type-assignable-p (this other)
  "Check if THIS accepts OTHER.

Uses special rules for `elsa-type-mixed'.

- Mixed accepts anything except unbound.
- Mixed is accepted by anything."
  (or (and (elsa-type-mixed-p this)
           (not (elsa-type-unbound-p other)))
      (elsa-type-mixed-p other)
      (elsa-type-accept this other)))

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

(cl-defgeneric elsa-type-get-return (_this)
  "Get return type of THIS type."
  nil)

(cl-defmethod elsa-type-get-return ((this elsa-type))
  this)

(cl-defmethod elsa-type-accept ((this elsa-type) other)
  "Test if THIS type accepts OTHER.

Accepting in this context means that OTHER can be assigned to
THIS."
  (cond
   ((elsa-instance-of other this))
   ((and (elsa-const-type-p other)
         (elsa-type-accept this (oref other type))))
   ((and (elsa-type-list-p this)
         (elsa-type-nil-p other)))
   ((elsa-type-composite-p other)
    (elsa-type-is-accepted-by other this))
   (t nil)))

;; (elsa-type-composite-p :: (function (mixed) bool))
(cl-defgeneric elsa-type-composite-p (_this)
  "Determine if the type is a composite type.

Composite types have to be wrapped in parens when passed as
arguments to other constructors."
  nil)

(cl-defmethod elsa-type-composite-p ((_this elsa-composite-type)) t)

(cl-defgeneric elsa-type-callable-p (_this)
  "Check if the type is callable.

A callable type can be called either directly as a list form or
with `funcall' or `apply' (or with other similar functions)."
  nil)

;; (elsa-function-type-nth-arg :: (function (mixed int) mixed))
(cl-defgeneric elsa-function-type-nth-arg (_this _n)
  "Return type of Nth argument.

For non-callable functions, return nil.

If N is more than the arity of the function and the last argument
is variadic, return that type, otherwise return nil."
  nil)

(defclass elsa-type-unbound (elsa-type) ()
  :documentation "Type of an unbound variable.

This is not accepted by any type because we don't know what it is.")

(defclass elsa-type-empty (elsa-type) ()
  :documentation "Empty type.  Has no domain.

This is accepted by any type and does not accept any type except
empty.")

(cl-defmethod elsa-type-describe ((_this elsa-type-empty))
  "empty")

(cl-defmethod elsa-type-accept ((this elsa-type-empty) other)
  (if (elsa-type-composite-p other)
      (elsa-type-is-accepted-by other this)
    (elsa-type-empty-p other)))

(cl-defmethod elsa-type-accept ((_this elsa-type) (_this2 elsa-type-empty))
  t)

(cl-defmethod elsa-type-accept ((_this elsa-type-unbound) _other)
  "Unbound type accepts anything.

The only thing that can be of an unbound type is a symbol
representing a variable.  It can accept anything because it is
not bound to any specific value yet."
  t)

(cl-defmethod elsa-type-describe ((_this elsa-type-unbound))
  "unbound")

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
        (new (cl-call-next-method this)))
    (oset new types types)
    new))

(cl-defmethod elsa-type-accept ((this elsa-intersection-type) other)
  (-all? (lambda (type) (elsa-type-accept type other)) (oref this types)))

(cl-defmethod elsa-type-is-accepted-by ((this elsa-intersection-type) other)
  (-any? (lambda (type) (elsa-type-accept other type)) (oref this types)))

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
        (new (cl-call-next-method this)))
    (oset new types types)
    new))

(cl-defmethod elsa-type-accept ((this elsa-sum-type) other)
  (cond
   ((elsa-type-composite-p other)
    (elsa-type-is-accepted-by other this))
   ((and (null (oref this types))
         (elsa-type-empty-p other)))
   (t
    (-any? (lambda (ot) (elsa-type-accept ot other)) (oref this types)))))

(cl-defmethod elsa-type-is-accepted-by ((this elsa-sum-type) other)
  (-all? (lambda (type) (elsa-type-accept other type)) (oref this types)))

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
  ((positive :initform (elsa-type-mixed) :initarg :positive)
   (negative :initform (elsa-type-empty) :initarg :negative))
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

(cl-defmethod elsa-type-accept ((this elsa-diff-type) other)
  (and (elsa-type-accept (oref this positive) other)
       (elsa-type-empty-p
        (elsa-type-intersect (oref this negative) other))))

(cl-defmethod elsa-type-is-accepted-by ((this elsa-diff-type) other)
  (elsa-type-accept other (oref this positive)))

(cl-defmethod elsa-type-describe ((this elsa-diff-type))
  (if (oref this negative)
      (format "(diff %s %s)"
              (elsa-type-describe (oref this positive))
              (elsa-type-describe (oref this negative)))
    (elsa-type-describe (oref this positive))))

(defclass elsa-type-t (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-t))
  "t")

(defclass elsa-type-nil (elsa-type) ())

(cl-defmethod elsa-type-accept ((_this elsa-type-nil) (other elsa-type))
  (elsa-type-nil-p other))

(cl-defmethod elsa-type-describe ((_this elsa-type-nil))
  "nil")

(defclass elsa-type-symbol (elsa-type) ()
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

(cl-defmethod elsa-type-accept ((_this elsa-type-bool) other)
  (or (elsa-type-bool-p other)
      (elsa-type-accept
       (elsa-sum-type
        :types (list (elsa-type-t) (elsa-type-nil)))
       other)))

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

(defclass elsa-type-string (elsa-type-sequence) ())

(cl-defmethod elsa-type-get-item-type ((_this elsa-type-string))
  "Get the type of items of a sequence type."
  (elsa-type-int))

(defclass elsa-type-short-string (elsa-type-string) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-string))
  "string")

(defclass elsa-type-buffer (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-buffer))
  "buffer")

(defclass elsa-type-frame (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-frame))
  "frame")

(defclass elsa-type-number (elsa-type)
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

(defclass elsa-type-marker (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-marker))
  "marker")

(defclass elsa-type-keyword (elsa-type-symbol) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-keyword))
  "keyword")

(defclass elsa-type-cons (elsa-type)
  ((car-type :type elsa-type :initarg :car-type
             :initform (elsa-type-mixed))
   (cdr-type :type elsa-type :initarg :cdr-type
             :initform (elsa-type-mixed))))

(cl-defmethod clone ((this elsa-type-cons))
  "Make a deep copy of a cons type."
  (let ((car-type (clone (oref this car-type)))
        (cdr-type (clone (oref this cdr-type)))
        (new (cl-call-next-method this)))
    (oset new car-type car-type)
    (oset new cdr-type cdr-type)
    new))

(cl-defmethod elsa-type-accept ((this elsa-type-cons) (other elsa-type-cons))
  "A cons type accepts another cons type covariantly.

That means that iff both arguments of this are supertypes of
other, then this is a supertype of other."
  (and (elsa-type-accept (oref this car-type) (oref other car-type))
       (elsa-type-accept (oref this cdr-type) (oref other cdr-type))))

(cl-defmethod elsa-type-is-accepted-by ((_this elsa-type-cons) (_other elsa-type-empty))
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
        (new (cl-call-next-method this)))
    (oset new types types)
    new))

(cl-defmethod elsa-type-accept ((this elsa-type-tuple) (other elsa-type-tuple))
  (catch 'ok
    ;; types are accepted co-variantly
    (let ((this-types (oref this types))
          (other-types (oref other types)))
      (unless (= (length this-types) (length other-types))
        (throw 'ok nil))
      (cl-mapc
       (lambda (this-type other-type)
         (unless (elsa-type-accept this-type other-type)
           (throw 'ok nil)))
       this-types other-types))
    t))

(cl-defmethod elsa-type-describe ((this elsa-type-tuple))
  (format "(%s)" (mapconcat 'elsa-type-describe (oref this types) " ")))

(defclass elsa-type-list (elsa-type-cons elsa-type-sequence)
  ((item-type :type elsa-type
              :initarg :item-type
              :initform (elsa-type-mixed))))

(cl-defmethod clone ((this elsa-type-list))
  "Make a deep copy of a list type."
  (let ((item-type (clone (oref this item-type)))
        (new (cl-call-next-method this)))
    (oset new item-type item-type)
    new))

(cl-defmethod elsa-type-accept ((this elsa-type-list) (other elsa-type))
  "A list type accepts another list type covariantly.

That means that iff the item type of this is supertype of other,
then this is a supertype of other.

Note that a list here is a homogeneous list, meaning a list of
unknown length where elements are all of the same type.

For a list of fixed length with heterogeneous types, see
`elsa-type-tuple'."
  (cond
   ((elsa-type-tuple-p other)
    ;; we can accept a tuple if all of its types are accepted by this
    (-all?
     (lambda (ot) (elsa-type-accept (oref this item-type) ot))
     (oref other types)))
   ((elsa-type-list-p other)
    (elsa-type-accept (oref this item-type) (oref other item-type)))
   (t (cl-call-next-method this other))))

(cl-defmethod elsa-type-describe ((this elsa-type-list))
  (format "(list %s)" (elsa-type-describe (oref this item-type))))

(cl-defmethod elsa-type-get-item-type ((this elsa-type-list))
  "Get the type of items of a sequence type."
  (oref this item-type))

(defclass elsa-type-vector (elsa-type-sequence)
  ((item-type :type elsa-type
              :initarg :item-type
              :initform (elsa-type-mixed))))

(cl-defmethod clone ((this elsa-type-vector))
  "Make a deep copy of a vector type."
  (let ((item-type (clone (oref this item-type)))
        (new (cl-call-next-method this)))
    (oset new item-type item-type)
    new))

(cl-defmethod elsa-type-accept ((this elsa-type-vector) (other elsa-type-vector))
  "A vector type accepts another vector type covariantly.

That means that iff the item type of this is supertype of other,
then this is a supertype of other."
  (elsa-type-accept (oref this item-type) (oref other item-type)))

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
        (new (cl-call-next-method this)))
    (oset new args args)
    (oset new return return)
    new))

(cl-defmethod elsa-type-describe ((this elsa-function-type))
  (format "(function (%s) %s)"
          (mapconcat 'elsa-type-describe (oref this args) " ")
          (elsa-type-describe (oref this return))))

(cl-defmethod elsa-type-accept ((this elsa-function-type) other)
  (if (elsa-type-composite-p other)
      (elsa-type-is-accepted-by other this)
    (when (elsa-function-type-p other)
      ;; Argument types must be contra-variant, return types must be
      ;; co-variant.
      (catch 'ok
        (let ((this-args (oref this args))
              (other-args (oref other args)))
          (unless (= (length this-args) (length other-args))
            (throw 'ok nil))
          (cl-mapc
           (lambda (this-arg other-arg)
             (unless (elsa-type-accept other-arg this-arg)
               (throw 'ok nil)))
           this-args other-args)
          (unless (elsa-type-accept
                   (elsa-type-get-return this)
                   (elsa-type-get-return other))
            (throw 'ok nil)))
        t))))

(cl-defmethod elsa-type-callable-p ((_this elsa-function-type)) t)

(cl-defmethod elsa-function-type-nth-arg ((this elsa-function-type) n)
  (let* ((args (oref this args))
         (type (nth n args)))
    (cond
     ((eq type nil)
      (let ((last-type (-last-item args)))
        (when (elsa-variadic-type-p last-type)
          (oref last-type item-type))))
     ((elsa-variadic-type-p type)
      (oref type item-type))
     (t type))))

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
        (new (cl-call-next-method this)))
    (oset new label label)
    new))

(cl-defmethod elsa-type-describe ((this elsa-generic-type))
  (symbol-name (oref this label)))

;; One-dimensional sparse arrays indexed by characters
(defclass elsa-type-char-table (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-char-table))
  "char-table")

;; One-dimensional arrays of t or nil.
(defclass elsa-type-bool-vector (elsa-type) ())

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
(defclass elsa-type-window (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-window))
  "window")

;; A terminal device displays frames
(defclass elsa-type-terminal (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-terminal))
  "terminal")

;; Recording the way a frame is subdivided
(defclass elsa-type-window-configuration (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-window-configuration))
  "window-configuration")

;; Recording the status of all frames
(defclass elsa-type-frame-configuration (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-frame-configuration))
  "frame-configuration")

;; A subprocess of Emacs running on the underlying OS
(defclass elsa-type-process (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-process))
  "process")

;; A thread of Emacs Lisp execution
(defclass elsa-type-thread (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-thread))
  "thread")

;; An exclusive lock for thread synchronization
(defclass elsa-type-mutex (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-mutex))
  "mutex")

;; Condition variable for thread synchronization
(defclass elsa-type-condition-variable (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-condition-variable))
  "condition-variable")

;; Receive or send characters
(defclass elsa-type-stream (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-stream))
  "stream")

;; What function a keystroke invokes
(defclass elsa-type-keymap (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-keymap))
  "keymap")

;; How an overlay is represented
(defclass elsa-type-overlay (elsa-type) ())

(cl-defmethod elsa-type-describe ((_this elsa-type-overlay))
  "overlay")

;; Fonts for displaying text
(defclass elsa-type-font (elsa-type) ())

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
        (new (cl-call-next-method this)))
    (oset new type type)
    new))

(cl-defmethod elsa-type-describe ((this elsa-const-type))
  (format "(const %S)" (oref this value)))

(cl-defmethod elsa-type-accept ((this elsa-const-type) other)
  "The const type is different from readonly in that a readonly
type can never be assigned to but a const type is only a
narrowing of a type to a concrete value from the type's domain."
  (and (elsa-const-type-p other)
       (elsa-type-accept (oref this type) (oref other type))
       (equal (oref this value) (oref other value))))

;; Readonly type for defconst
(defclass elsa-readonly-type (elsa-type elsa-composite-type)
  ((type :type elsa-type :initarg :type))
  :documentation "Read-only (or constant) type.

It wraps any other type and makes the form or variable
unassignable.")

(cl-defmethod elsa-type-accept ((this elsa-readonly-type) other)
  "Check if this readonly type accept other type.

The acceptance does not depend on the readonly status.  Here we
simply work with the wrapped types.  The readonly type wrapper is
only a flag which is used during assignability checks (such as
`setq'), not as a type check!"
  (if (elsa-type-composite-p other)
      (elsa-type-is-accepted-by other (oref this type))
    (elsa-type-accept (oref this type) other)))

(cl-defmethod elsa-type-is-accepted-by ((this elsa-readonly-type) other)
  (elsa-type-accept other (oref this type)))

(cl-defmethod elsa-type-describe ((this elsa-readonly-type))
  (format "(readonly %s)" (elsa-type-describe (oref this type))))

(cl-defmethod elsa-type-accept ((_this null) _other)
  "This method catches the impossible situation when we are
trying to analyse 'nil, which is not a valid type or form."
  (message "An error happened trying to analyse nil")
  (backtrace))

(defclass elsa-struct-type (elsa-type)
  ((name :type (or symbol nil) :initarg :name))
  :documentation "Type representing a `cl-defstruct' or `defclass'.")

(cl-defmethod elsa-type-accept ((this elsa-struct-type) other)
  (cond
   ((elsa-type-composite-p other)
    (elsa-type-is-accepted-by other this))
   ((elsa-struct-type-p other)
    (if (not (oref this name))
        t
      (-when-let* ((this-struct (get (oref this name) 'elsa-cl-structure))
                   (other-struct (get (oref other name) 'elsa-cl-structure)))
        (not (null (assq (oref this-struct name) (oref other-struct parents)))))))))

(cl-defmethod elsa-type-describe ((this elsa-struct-type))
  (format "(struct %s)" (oref this name)))

(provide 'elsa-types)
;;; elsa-types.el ends here
