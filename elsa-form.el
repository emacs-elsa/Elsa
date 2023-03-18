;; -*- lexical-binding: t -*-

(require 'trinary)

(require 'elsa-methods)
(require 'elsa-types-simple)

(defclass elsa-form nil
  ((start :type integer :initarg :start)
   (end :type integer :initarg :end)
   (quote-type :type symbol :initarg :quote-type :initform nil)
   (line :type integer :initarg :line)
   (column :type integer :initarg :column)
   (end-line :type integer :initarg :end-line)
   (end-column :type integer :initarg :end-column)
   (type :type elsa-type :initarg :type :initform (elsa-type-mixed))
   (narrow-types :initarg :narrow-type :initform nil)
   (reachable :type trinary :initarg :reachable :initform (trinary-true))
   (parent
    :type (or elsa-form null)
    :initarg :parent
    :documentation "Parent form.")
   (previous
    :type (or elsa-form null)
    :initarg :previous
    :documentation "Previous form in a sequence.")
   (was-expanded
    :type boolean
    :initform nil
    :documentation "Was this form a macro call form which was expanded.

This is only set on the macro call form which was expanded, not on the
child forms.")
   (expanded-form
    :type (or elsa-form null)
    :initform nil
    :documentation "The form corresponding to this form in macroexpanded subtree.")
   (original-form
    :type (or elsa-form null)
    :initform nil
    :documentation "The form from which this form was macroexpanded.")
   (annotation :type list :initarg :annotation :initform nil))
  :abstract t)

(cl-defgeneric elsa-form-print ((this elsa-form))
  "Print THIS form in a way that can be read back in.

This function does not prettyprint.

Each class should implement more efficient print method if
possible since format has some overhead parsing the specification
and so on."
  (format "%s" this))

(cl-defgeneric elsa-form-to-lisp ((this elsa-form))
  "Return this form as lisp form."
  (error "Not implemented for form: %S" this))

(cl-defgeneric elsa-form-visit ((this elsa-form) fn)
  "Visit each node of THIS elsa-form and call FN.

If the form is a cons, list or vector, recurse into the child
nodes."
  (declare (indent 1))
  (funcall fn this))

(cl-defgeneric elsa-form-foreach (_elsa-form _fn)
  "For each item of ELSA-FORM execute FN with the item as first argument.

This only makes sense for the sequence forms:

- `elsa-form-vector'
- `elsa-form-list'
- `elsa-form-improper-list'"
  (declare (indent 1))
  nil)

(cl-defgeneric elsa-form-map (_elsa-form _fn)
  "Map each item of ELSA-FORM through FN with the item as first argument.

This only makes sense for the sequence forms:

- `elsa-form-vector'
- `elsa-form-list'
- `elsa-form-improper-list'"
  (declare (indent 1))
  nil)

(cl-defgeneric elsa-form-sequence-p (_this)
  nil)

;; (elsa-form-function-call-p :: (function (mixed (or symbol nil)) bool))
(cl-defgeneric elsa-form-function-call-p (_this &optional _name) nil)

;; (elsa-cadr :: (function (mixed) mixed))
(cl-defgeneric elsa-cadr (thing)
  "Return `cadr' of THING")

(defun elsa-form-find-parent (form pred)
  "Find first parent of FORM satisfying predicate PRED."
  (declare (indent 1))
  (let ((parent form))
    (while (and parent (not (funcall pred parent)))
      (setq parent (and (slot-boundp parent 'parent)
                        (oref parent parent))))
    parent))

(defun elsa-form-find-child (form pred)
  "Find first child of FORM satisfying predicate PRED."
  (declare (indent 1))
  (let (re)
    (catch 'found
      (elsa-form-visit form
        (lambda (child)
          (when (funcall pred child)
            (setq re child)
            (throw 'found child)))))
    re))

(defun elsa-locate-dominating-form (form name)
  "Starting at FORM, look up parent forms for form with NAME.

NAME can be a symbol or list of symbols, in which case matching
any symbol from the list will stop the search."
  (elsa-form-find-parent
   form
   (lambda (parent)
     (if (listp name)
         (memq (elsa-get-name parent) name)
       (eq (elsa-get-name parent) name)))))

(provide 'elsa-form)
