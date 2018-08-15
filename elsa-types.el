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

(defclass elsa-type nil () :abstract t)

;; (elsa-type-format-arg :: Mixed -> String)
(defun elsa-type-format-arg (type)
  "Format TYPE as an argument for `elsa-make-type'.

First describe TYPE using `elsa-type-describe'.  If TYPE is
composite also wrap it in parens to preserve the semantics when
it is passed to another constructor."
  (let ((printed (elsa-type-describe type)))
    (if (and (elsa-type-composite-p type)
             (not (elsa-type-list-p type)))
        (format "(%s)" printed)
      printed)))

(cl-defmethod elsa-type-describe ((this elsa-type))
  "Describe THIS type."
  (symbol-name (eieio-object-class this)))

(cl-defmethod elsa-type-get-args (this)
  "Get argument types of THIS type."
  nil)

(cl-defmethod elsa-type-get-return (this)
  "Get return type of THIS type."
  nil)

(cl-defmethod elsa-type-get-return ((this elsa-type))
  "Get return type of THIS type."
  this)

(cl-defmethod elsa-type-accept ((this elsa-type) other)
  "Test if THIS type accepts OTHER.

Accepting in this context means that OTHER can be assigned to
THIS."
  (cond
   ((elsa-instance-of other this))
   ((and (elsa-sum-type-p other)
         (-all? (lambda (other-type)
                  (elsa-type-accept this other-type))
                (oref other types))))
   (t nil)))

(cl-defmethod elsa-type-composite-p ((this elsa-type))
  "Determine if the type is a composite type.

Composite types have to be wrapped in parens when passed as
arguments to other constructors."
  nil)

(cl-defmethod elsa-type-restrict-by ((this elsa-type) other)
  (error "Not implemented yet"))

(cl-defmethod elsa-type-make-non-nullable ((this elsa-type))
  (error "Not implemented yet"))

(defclass elsa-type-unbound (elsa-type) ()
  :documentation "Type of an unbound variable.

This is not accepted by any type because we don't know what it is.")

(cl-defmethod elsa-type-accept ((this elsa-type-unbound) other)
  "Unbount type accepts anything.

The only thing that can be of an unbound type is a symbol
representing a variable.  It can accept anything because it is not bound to any specific value yet."
  t)

(cl-defmethod elsa-type-describe ((this elsa-type-unbound))
  "Unbound")

(defclass elsa-sum-type (elsa-type)
  ((types :type list
          :initarg :types
          :initform nil))
  :documentation "Sum type.

This type is a combination of other types.  It can accept any
type that is accepted by at least one of its summands.")

(cl-defmethod elsa-type-describe ((this elsa-sum-type))
  (cond
   ;; TODO: this should be really handled by the normalization step, a
   ;; sum type Mixed | <...> => Mixed, possibly without Nil
   ((elsa-type-accept this (elsa-type-mixed))
    "Mixed")
   ((and (= 2 (length (oref this types)))
         (elsa-type-nil-p (car (oref this types))))
    (concat (elsa-type-format-arg (cadr (oref this types))) "?"))
   ((and (= 2 (length (oref this types)))
         (elsa-type-nil-p (cadr (oref this types))))
    (concat (elsa-type-format-arg (car (oref this types))) "?"))
   (t
    (mapconcat 'elsa-type-format-arg (oref this types) " | "))))

(cl-defmethod elsa-type-composite-p ((this elsa-sum-type)) t)

(cl-defmethod clone ((this elsa-sum-type))
  "Make a deep copy of a sum type."
  (let ((types (-map 'clone (oref this types)))
        (new (cl-call-next-method this)))
    (oset new types types)
    new))

;; TODO: handle sum types and diff types as arguments
(cl-defmethod elsa-sum-type-remove ((this elsa-sum-type) other)
  (unless (elsa-type-child-p other) (error "Other must be `elsa-type-child-p'"))
  (oset this types (--remove (eq (eieio-object-class it)
                                 (eieio-object-class other))
                             (oref this types))))

(cl-defmethod elsa-type-make-non-nullable ((this elsa-sum-type))
  (elsa-sum-type-remove this (elsa-type-nil)))

(cl-defmethod elsa-type-accept ((this elsa-sum-type) other)
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

(cl-defmethod elsa-diff-type-remove-positive ((this elsa-diff-type) other)
  (unless (elsa-type-child-p other) (error "Other must be `elsa-type-child-p'"))
  (if (elsa-type-nil-p other)
      (elsa-type-make-non-nullable (oref this positive))
    (elsa-sum-type-remove (oref this positive) other)))

(defclass elsa-type-t (elsa-type) ())

(cl-defmethod elsa-type-describe ((this elsa-type-t))
  "T")

(defclass elsa-type-nil (elsa-type) ())

(cl-defmethod elsa-type-accept ((this elsa-type-nil) other)
  (elsa-type-nil-p other))

(cl-defmethod elsa-type-describe ((this elsa-type-nil))
  "Nil")

(defclass elsa-type-symbol (elsa-type) ()
  :documentation "Quoted symbol")

(cl-defmethod elsa-type-describe ((this elsa-type-symbol))
  "Symbol")

(defclass elsa-type-bool (elsa-type elsa-type-symbol) ())

(cl-defmethod elsa-type-accept ((this elsa-type-bool) other)
  (or (elsa-type-bool-p other)
      (elsa-type-accept (elsa-make-type T?) other)))

(cl-defmethod elsa-type-describe ((this elsa-type-bool))
  "Bool")

;; Mixed type is special in that it is always created nullable.  Mixed
;; can also serve as bool type in Emacs Lisp.
(defclass elsa-type-mixed (elsa-type) ())

(cl-defmethod elsa-type-describe ((this elsa-type-mixed))
  "Mixed")

(cl-defmethod elsa-type-accept ((this elsa-type-mixed) other)
  (unless (elsa-type-child-p other) (error "Other must be `elsa-type-child-p'"))
  (not (eq (eieio-object-class other) 'elsa-type-unbound)))

(defclass elsa-type-sequence (elsa-type) ())

(cl-defmethod elsa-type-get-item-type ((this elsa-type))
  "Get the type of items of a sequence type."
  nil)

(defclass elsa-type-string (elsa-type-sequence) ())

(cl-defmethod elsa-type-get-item-type ((this elsa-type-string))
  "Get the type of items of a sequence type."
  (elsa-type-int))

(defclass elsa-type-short-string (elsa-type-string) ())

(cl-defmethod elsa-type-describe ((this elsa-type-string))
  "String")

(defclass elsa-type-buffer (elsa-type) ())

(cl-defmethod elsa-type-describe ((this elsa-type-buffer))
  "Buffer")

(defclass elsa-type-frame (elsa-type) ())

(cl-defmethod elsa-type-describe ((this elsa-type-frame))
  "Frame")

(defclass elsa-type-number (elsa-type) ())

(cl-defmethod elsa-type-describe ((this elsa-type-number))
  "Number")

(defclass elsa-type-int (elsa-type-number) ())

(cl-defmethod elsa-type-describe ((this elsa-type-int))
  "Int")

(defclass elsa-type-float (elsa-type-number) ())

(cl-defmethod elsa-type-describe ((this elsa-type-float))
  "Float")

(defclass elsa-type-marker (elsa-type) ())

(cl-defmethod elsa-type-describe ((this elsa-type-marker))
  "Marker")

(defclass elsa-type-keyword (elsa-type) ())

(cl-defmethod elsa-type-describe ((this elsa-type-keyword))
  "Keyword")

(defclass elsa-type-cons (elsa-type)
  ((car-type :type elsa-type :initarg :car-type
             :initform (elsa-type-mixed))
   (cdr-type :type elsa-type :initarg :cdr-type
             :initform (elsa-type-mixed))))

(cl-defmethod elsa-type-accept ((this elsa-type-cons) (other elsa-type-cons))
  "A cons type accepts another cons type covariantly.

That means that iff both arguments of this are supertypes of
other, then this is a supertype of other."
  (and (elsa-type-accept (oref this car-type) (oref other car-type))
       (elsa-type-accept (oref this cdr-type) (oref other cdr-type))))

(cl-defmethod elsa-type-composite-p ((this elsa-type-cons)) t)

(cl-defmethod elsa-type-describe ((this elsa-type-cons))
  (format "Cons %s %s"
          (elsa-type-format-arg (oref this car-type))
          (elsa-type-format-arg (oref this cdr-type))))

(defclass elsa-type-list (elsa-type-cons elsa-type-sequence)
  ((item-type :type elsa-type
              :initarg :item-type
              :initform (elsa-type-mixed))))

(cl-defmethod elsa-type-describe ((this elsa-type-list))
  (format "[%s]" (elsa-type-describe (oref this item-type))))

(cl-defmethod elsa-type-composite-p ((this elsa-type-list)) t)

(cl-defmethod elsa-type-get-item-type ((this elsa-type-list))
  "Get the type of items of a sequence type."
  (oref this item-type))

(defclass elsa-type-vector (elsa-type-sequence)
  ((item-type :type elsa-type
              :initarg :item-type
              :initform (elsa-type-mixed))))

(cl-defmethod elsa-type-composite-p ((this elsa-type-vector)) t)

(cl-defmethod elsa-type-describe ((this elsa-type-vector))
  (format "Vector %s" (elsa-type-format-arg (oref this item-type))))

(cl-defmethod elsa-type-get-item-type ((this elsa-type-vector))
  "Get the type of items of a sequence type."
  (oref this item-type))

(defclass elsa-variadic-type (elsa-type-list) nil)

(cl-defmethod elsa-type-describe ((this elsa-variadic-type))
  (format "%s..." (elsa-type-format-arg (oref this item-type))))

(defclass elsa-function-type (elsa-type)
  ((args :type list :initarg :args)
   (return :type elsa-type :initarg :return)))

(cl-defmethod elsa-type-describe ((this elsa-function-type))
  (mapconcat (lambda (type)
               (let ((printed (elsa-type-describe type)))
                 (if (elsa-function-type-p type)
                     (format "(%s)" printed)
                   printed)))
             (-snoc (oref this args) (oref this return))
             " -> "))

(cl-defmethod elsa-type-accept ((this elsa-function-type) other)
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
      t)))

(cl-defmethod elsa-type-composite-p ((this elsa-function-type)) t)

;; (elsa-function-type-nth-arg :: Int -> Mixed -> Mixed)
(defun elsa-function-type-nth-arg (n elsa-type)
  (let* ((args (oref elsa-type args))
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
  "Get argument types of THIS type."
  (oref this args))

(cl-defmethod elsa-type-get-return ((this elsa-function-type))
  "Get return type of THIS type."
  (oref this return))

(defclass elsa-generic-type (elsa-type)
  ((label :type symbol :initarg :label)))

(cl-defmethod elsa-type-describe ((this elsa-generic-type))
  (symbol-name (oref this label)))

;; TODO: add a function type, then use it in defuns and variables
;; which are lambdas

(provide 'elsa-types)
;;; elsa-types.el ends here
