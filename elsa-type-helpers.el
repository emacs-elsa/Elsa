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
(eval-and-compile (setq eieio-backward-compatibility nil))

(require 'dash)
(require 'trinary)

(require 'elsa-types)
(require 'elsa-type-algebra)

;; (elsa-type-is-nil :: (function (mixed) (struct trinary)))
(cl-defgeneric elsa-type-is-nil (type)
  "Test if TYPE is always nil.

Return trinary logic value.")

;; TODO: use elsa-type-could-accept or non-nil and trinary-not
(cl-defmethod elsa-type-is-nil ((type elsa-type))
  (elsa-type-could-accept (elsa-type-nil) type))

;; (elsa-type-is-non-nil :: (function (mixed) (struct trinary)))
(cl-defgeneric elsa-type-is-non-nil (type)
  "Test if TYPE is always non-nil.

Return trinary logic value.")

;; TODO: use elsa-type-could-accept
(cl-defmethod elsa-type-is-non-nil ((type elsa-type))
  (trinary-not (elsa-type-could-accept (elsa-type-nil) type)))

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

(defun elsa-type-could-assign-p (this other)
  "Check if THIS could accept OTHER.

Uses special rules for `elsa-type-mixed'.

- Mixed accepts anything except unbound.
- Mixed is accepted by anything.

Returns trinary value."
  (trinary-or
   (trinary-from-bool
    (or (and (elsa-type-mixed-p this)
             (not (elsa-type-unbound-p other)))
        (elsa-type-mixed-p other)))
   (elsa-type-could-accept this other)))

(defun elsa--make-const-type (value)
  "Construct const type based on VALUE."
  (cond
   ((keywordp value)
    (elsa-const-type :type (elsa-type-keyword) :value value))
   ((stringp value)
    (elsa-const-type :type (elsa-type-string) :value value))
   ((integerp value)
    (elsa-const-type :type (elsa-type-int) :value value))
   ((floatp value)
    (elsa-const-type :type (elsa-type-float) :value value))
   ((symbolp value)
    (elsa-const-type :type (elsa-type-symbol) :value value))
   (t (error "Trying to make a const type out of %S" value))))

(defun elsa--make-union-type (definition)
  (->> definition
       (-map 'elsa--make-type)
       (-reduce 'elsa-type-sum)))

(defun elsa--make-intersection-type (definition)
  (->> definition
       (-map 'elsa--make-type)
       (-reduce 'elsa-type-intersect)))

(defun elsa--create-type-error (definition)
  (let ((could-be-composite-p
         (and (symbolp definition)
              (memq definition '(function and or diff struct const
                                          vector list cons readonly)))))
    (error "Unknown Elsa type %s%s"
           definition
           (if could-be-composite-p
               " (maybe there are missing parentheses around the type constructor)"
             ""))))

(defun elsa--make-type (definition)
  (pcase definition
    (`(edebug-after ,_ ,_ ,form)
     (elsa--make-type form))
    (`(readonly ,type)
     (elsa-readonly-type :type (elsa--make-type type)))
    ((or `(const ,value) `(quote ,value))
     (elsa--make-const-type value))
    (`(cons) ;; mixed cons by default
     (elsa-type-cons :car-type (elsa-type-mixed)
                     :cdr-type (elsa-type-mixed)))
    (`(vector) ;; mixed vector by default
     (elsa-type-vector :item-type (elsa-type-mixed)))
    (`(list) ;; mixed list by default
     (let* ((item-type (elsa-type-mixed))
            (list-type (elsa-type-list :item-type item-type)))
       (oset list-type car-type item-type)
       (oset list-type cdr-type item-type)
       list-type))
    (`(or . ,def)
     (elsa--make-union-type def))
    (`(and . ,def)
     (elsa--make-intersection-type def))
    (`(diff ,positive ,negative)
     (elsa-type-normalize
      (elsa-diff-type :positive (elsa--make-type positive)
                      :negative (elsa--make-type negative))))
    (`(cons ,a ,b)
     (elsa-type-cons :car-type (elsa--make-type a)
                     :cdr-type (elsa--make-type b)))
    (`(list ,a)
     (let* ((item-type (elsa--make-type a))
            (list-type (elsa-type-list :item-type item-type)))
       (oset list-type car-type item-type)
       (oset list-type cdr-type item-type)
       list-type))
    ((and `(,type . ,slots)
          (guard (memq type '(plist keys))))
     (let* ((decl (-split-on '&extends slots))
            (slot-pairs (-partition 2 (unless (eq '&extends (car slots))
                                        (car decl))))
            (interfaces (if (eq '&extends (car slots))
                            (car decl)
                          (cadr decl)))
            (table (make-hash-table)))
       (-each slot-pairs
         (lambda (slot)
           (puthash (car slot)
                    (elsa-structure-slot
                     :name (car slot)
                     :type (elsa--make-type (cadr slot)))
                    table)))
       (if (eq type 'plist)
           (elsa-type-plist :slots table
                            :extends interfaces)
         (elsa-type-keys :slots table))))
    (`(interface ,name . ,slots)
     (let* ((decl (-split-on '&extends slots))
            (slot-pairs (-partition 2 (unless (eq '&extends (car slots))
                                        (car decl))))
            (interfaces (if (eq '&extends (car slots))
                            (car decl)
                          (cadr decl)))
            (table (make-hash-table)))
       (-each slot-pairs
         (lambda (slot)
           (puthash (car slot)
                    (elsa-structure-slot
                     :name (car slot)
                     :type (elsa--make-type (cadr slot)))
                    table)))
       (elsa-interface :name name
                       :slots table
                       :extends interfaces)))
    (`(&rest ,a)
     (let* ((item-type (elsa--make-type a))
            (variadic-type (elsa-variadic-type :item-type item-type)))
       variadic-type))
    (`(vector ,a)
     (let* ((item-type (elsa--make-type a))
            (vector-type (elsa-type-vector :item-type item-type)))
       vector-type))
    (`(struct ,name)
     (elsa-struct-type :name name))
    (`(class ,name)
     (elsa-class-type :name name))
    (`(cl-ref ,name)
     (elsa-type--cl-ref :name name))
    (`(function ,args ,ret)
     (elsa-function-type
      :args (-map 'elsa--make-type
                  (-let* (((positional rest)
                           (-split-with (lambda (x) (not (equal x '&rest))) args)))
                    (append positional (when rest (list rest)))))
      :return (elsa--make-type ret)))
    (`(is ,type)
     (elsa-type-bool :predicates (elsa--make-type type)))
    ((and `,arg (guard (symbolp arg)))
     (let* ((type-name (symbol-name arg))
            (constructor (intern (concat "elsa-type-" type-name))))
       (cond
        ((functionp constructor)
         (funcall constructor))
        (t (elsa--create-type-error arg)))))
    ((and `,arg (guard (or (keywordp arg)
                           (stringp arg)
                           (integerp arg)
                           (floatp arg))))
     (elsa--make-const-type arg))
    ;; this must be after the symbol check because nil is also a list
    ;; but also can be constructor for `elsa-type-nil'.
    ((and (pred listp) args)
     (elsa-type-tuple :types (-map #'elsa--make-type args)))
    (_ (elsa--create-type-error definition))))

(defmacro elsa-make-type (&rest definition)
  "Make a type according to DEFINITION."
  (declare (debug (&rest sexp)))
  `(elsa--make-type ',@definition))

(defun elsa--cl-type-to-elsa-type (definition)
  (pcase definition
    (`nil
     (elsa-type-mixed))
    (`t
     (elsa-type-mixed))
    (`null
     (elsa-type-nil))
    (`integer
     (elsa-type-int))
    (`boolean
     (elsa-type-bool))
    ((pred symbolp)
     (or
      ;; It can be a primitive type, which should be represented as an
      ;; elsa type with "elsa-type" prefix.
      (ignore-errors (elsa--make-type definition))
      ;; It can also be a cl-struct name.  So we try to construct an
      ;; elsa struct type from it.
      (and (get definition 'elsa-defstruct)
           (elsa--make-type
            `(struct ,definition)))
      (and (get definition 'elsa-defclass)
           (elsa--make-type
            `(class ,definition)))
      (elsa-type--cl-ref :name definition)))
    ;; list of "cl-type"
    (`(list-of ,type)
     (elsa-type-list :item-type (or (elsa--cl-type-to-elsa-type type)
                                    (elsa-type-mixed))))
    (`(or . ,type)
     (elsa-sum-type :types (--map (elsa--cl-type-to-elsa-type it) type)))
    (`(and . ,type)
     (elsa-intersection-type :types (--map (elsa--cl-type-to-elsa-type it) type)))
    (`(not ,type)
     (elsa-diff-type :positive (elsa-type-mixed)
                     :negative (elsa--cl-type-to-elsa-type type)))))

(defun elsa--eieio-class-parents-recursive (type)
  "Return all parents of TYPE."
  (cons type
        (-mapcat 'elsa--eieio-class-parents-recursive
                 (-map 'eieio-class-name (eieio-class-parents type)))))

(defun elsa-type-equivalent-p (this other)
  "Test if THIS and OTHER are equivalent types."
  (and (elsa-type-accept (elsa-get-type this) (elsa-get-type other))
       (elsa-type-accept (elsa-get-type other) (elsa-get-type this))))

(defun elsa--poset-sort (comp list)
  "Use COMP to poset-order LIST.

Return a list of lists where each chain is ordered and different
chains' heads are not comparable."
  (setq list (-sort comp list))
  (let ((chains (list (list (car list))))
        (comp-inv (-flip comp)))
    (-each (cdr list)
      (lambda (item)
        (unless (catch 'pushed
                  (-each-indexed chains
                    (-lambda (index (chain &as head))
                      (when (funcall comp-inv item head)
                        (push item chain)
                        (setq chains (-replace-at index chain chains))
                        (throw 'pushed t)))))
          (push (list item) chains))))
    (mapcar #'reverse chains)))

(defun elsa--equivalence-classes (comp list)
  "Use COMP to group types in LIST into equivalence classes.

COMP should be an equivalence relation, otherwise the results
might not make sense."
  (let ((groups (list (list (car list)))))
    (-each (cdr list)
      (lambda (item)
        (unless (catch 'pushed
                  (-each-indexed groups
                    (-lambda (index (group &as head))
                      (when (funcall comp item head)
                        (push item group)
                        (setq groups (-replace-at index group groups))
                        (throw 'pushed t)))))
          (push (list item) groups))))
    groups))

(defun elsa--simplify-overloads (list)
  "Select most specific overloads from LIST.

If overloads are not comparable, select all of them.

Items in LIST are of form (TYPE OVERLOAD-INDEX INDEX).

INDEX is the position in the arglist or a keyword for named
arguments."
  (let ((parts (--separate (nth 2 it) list)))
    (append
     (cadr parts)
     (-mapcat
      #'car
      (elsa--poset-sort
       ;; we order the equivalence classes by the first element of the
       ;; group
       (lambda (a b)
         (let ((first (car a))
               (second (car b)))
           (elsa-type-accept
            (elsa-function-type-nth-arg (car second) (nth 2 second))
            (elsa-function-type-nth-arg (car first) (nth 2 first)))))
       (elsa--equivalence-classes
        (lambda (a b)
          (elsa-type-equivalent-p
           (elsa-function-type-nth-arg (car b) (nth 2 b))
           (elsa-function-type-nth-arg (car a) (nth 2 a))))
        (car parts)))))))

(defun elsa-type-is-empty-p (this)
  "Test if THIS is type-equivalent to `elsa-type-empty'.

This means that the domain of the type is empty."
  (elsa-type-accept (elsa-type-empty) this))

(defun elsa-type-nullable-p (type)
  "Test if TYPE is nullable (i.e. accepts nil)."
  (elsa-type-accept type (elsa-make-type nil)))

(defun elsa-type-make-nullable (type)
  "Make TYPE nullable."
  (elsa-type-sum type (elsa-make-type nil)))

(cl-defmethod elsa-type-make-non-nullable ((type elsa-type))
  "Make TYPE non-nullable."
  (elsa-type-diff type (elsa-make-type nil)))

(cl-defmethod elsa-type-normalize ((this elsa-type))
  "Normalize a type.

Regular type normalizes to itself."
  this)

(cl-defmethod elsa-type-normalize ((this elsa-readonly-type))
  "Normalize a readonly type.

A readonly of readonly can be squashed to just readonly."
  (if (elsa-readonly-type-p (oref this type))
      (elsa-type-normalize (oref this type))
    (elsa-readonly-type :type (elsa-type-normalize (oref this type)))))

(cl-defmethod elsa-type-normalize ((this elsa-sum-type))
  "Normalize a sum type."
  (elsa-type-debug ("(elsa-type-normalize %s) elsa-sum-type" this)
    (if (--any? (elsa-type-mixed-p it) (oref this types))
        (elsa-type-mixed)
      (let ((types (--remove (elsa-type-is-empty-p it) (oref this types))))
        (cond
         ((not types)
          (elsa-type-empty))
         ((= 1 (length types))
          (car types))
         (t (elsa-sum-type :types types)))))))

(cl-defmethod elsa-type-normalize ((this elsa-intersection-type))
  "Normalize an intersection type."
  (elsa-type-debug ("(elsa-type-normalize %s) elsa-intersection-type" this)
    (let ((types (--remove (elsa-type-accept it (elsa-make-type mixed))
                           (oref this types))))
      (cond
       ((= 1 (length types))
        (car types))
       (t (elsa-intersection-type :types types))))))

(cl-defmethod elsa-type-normalize ((this elsa-diff-type))
  "Normalize a diff type."
  (elsa-type-debug ("(elsa-type-normalize %s) elsa-diff-type" this)
    (let ((pos (oref this positive))
          (neg (oref this negative)))
      (cond
       ((elsa-type-equivalent-p pos neg)
        (elsa-type-empty))
       ((elsa-type-is-empty-p neg)
        (clone pos))
       ((and (elsa-type-number-p pos)
             (elsa-type-float-p neg))
        (elsa-type-int))
       ((and (elsa-type-number-p pos)
             (elsa-type-int-p neg))
        (elsa-type-float))
       ((and (not (elsa-type-composite-p pos))
             (elsa-diff-type-p neg))
        (elsa-type-diff pos neg))
       ((elsa-type-mixed-p neg)
        (elsa-type-empty))
       (t (let ((intersect-pos-neg (elsa-type-intersect pos neg)))
            (cond
             ((elsa-type-is-empty-p intersect-pos-neg)
              (clone pos))
             (t (cond
                 ((elsa-type-equivalent-p intersect-pos-neg neg)
                  (clone this))
                 ((elsa-type-equivalent-p pos intersect-pos-neg)
                  (elsa-type-empty))
                 (t
                  (elsa-diff-type :positive pos
                                  :negative intersect-pos-neg)))))))))))

(provide 'elsa-type-helpers)
;;; elsa-type-helpers.el ends here
