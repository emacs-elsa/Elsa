;;; elsa-type-algebra.el --- Elsa type algebra -*- lexical-binding: t -*-

(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))
(require 'elsa-types)

(eval-and-compile
  (defvar elsa-type-debug nil))

(defvar elsa-type-debug-depth 0)

(defmacro elsa-type-debug (msg-spec &rest body)
  (declare (indent 1)
           (debug ((stringp form &optional form) body)))
  (if elsa-type-debug
      `(progn
         (cl-incf elsa-type-debug-depth)
         (message (concat (make-string elsa-type-debug-depth 32)
                          ,(car msg-spec))
                  ,@(mapcar (lambda (x) `(elsa-type-describe ,x)) (cdr msg-spec)))
         (prog1 ,@body
           (cl-decf elsa-type-debug-depth)))
    `(progn ,@body)))

(cl-defmethod elsa-type-sum ((this elsa-type) (other elsa-type))
  "Basic primitive type sum."
  (elsa-type-debug ("(elsa-type-sum %s %s) elsa-type elsa-type " this other)
    (elsa-type-normalize
     (cond
      ((or (and (elsa-type-nil-p this) (elsa-type-t-p other))
           (and (elsa-type-t-p this) (elsa-type-nil-p other)))
       (elsa-type-bool))
      ((elsa-readonly-type-p this)
       (elsa-type-normalize
        (elsa-readonly-type :type (elsa-type-sum (oref this type) other))))
      ((elsa-type-composite-p other)
       (elsa-type-sum other this))
      ((elsa-type-accept this other)
       (clone this))
      ((elsa-type-accept other this)
       (clone other))
      ((and (not (elsa-type-composite-p this))
            (not (elsa-type-composite-p other)))
       (elsa-sum-type :types (list this other)))
      (t (let ((sum (elsa-sum-type :types (list this))))
           (elsa-type-sum sum other)))))))

(cl-defmethod elsa-type-sum ((this elsa-readonly-type) (other elsa-readonly-type))
  (elsa-type-debug ("(elsa-type-sum %s %s) elsa-readonly-type elsa-readonly-type" this other)
    (elsa-readonly-type :type (elsa-type-sum (oref this type) (oref other type)))))

(cl-defmethod elsa-type-sum ((this elsa-type-number) (other elsa-type-number))
  "Numbers are handled in special way, see `elsa-type-number'"
  (elsa-type-debug ("(elsa-type-sum %s %s) elsa-type-number elsa-type-number" this other)
    (cond
     ((and (elsa-type-float-p this) (elsa-type-int-p other))
      (elsa-type-number))
     ((and (elsa-type-int-p this) (elsa-type-float-p other))
      (elsa-type-number))
     (t (cl-call-next-method)))))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-sum-type))
  "(A ∪ B ∪ ...) ∪ (C ∪ D ∪ ...) = A ∪ B ∪ C ∪ D ∪ ...

This is only for performance reasons, because we know we are
combining two unions, we don't have to unpack each argument of
first union against each of the second."
  (elsa-type-debug ("(elsa-type-sum %s %s) elsa-sum-type elsa-sum-type" this other)
    (elsa-type-normalize
     (let ((new-other nil)
           (new-this nil))
       ;; remove unnecessary types from other
       (-each (oref other types)
         (lambda (type)
           (unless (elsa-type-accept this type)
             (push (clone type) new-other))))
       ;; remove unnecessary types from this
       (let ((new-sum (elsa-sum-type :types new-other)))
         (-each (oref this types)
           (lambda (type)
             (unless (elsa-type-accept new-sum type)
               (push (clone type) new-this)))))
       (elsa-sum-type :types (-concat (nreverse new-this) (nreverse new-other)))))))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-diff-type))
  "(A ∪ B) ∪ (C - D) = (A ∪ B ∪ C) - D"
  (elsa-type-debug ("(elsa-type-sum %s %s) elsa-sum-type elsa-diff-type" this other)
    (elsa-type-normalize
     (let ((diff-pos (oref other positive))
           (diff-neg (oref other negative)))
       (elsa-type-diff (elsa-type-sum this diff-pos) diff-neg)))))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-type))
  "(A ∪ B) ∪ C = (A ∪ C) ∪ (B ∪ C)"
  (elsa-type-debug ("(elsa-type-sum %s %s) elsa-sum-type elsa-type" this other)
    (elsa-type-normalize
     (cond
      ((not (elsa-type-composite-p other))
       (elsa-type-sum this (elsa-sum-type :types (list other))))
      ((elsa-type-composite-p other)
       (-reduce
        #'elsa-type-sum
        (-map (lambda (type) (elsa-type-sum type other)) (oref this types))))
      (t
       (if (elsa-type-accept this other)
           (clone this)
         (elsa-type-sum this (elsa-sum-type :types (list other)))))))))

(cl-defmethod elsa-type-sum ((this elsa-diff-type) (other elsa-type))
  "(A - B) ∪ C = (A ∪ C) - (B - C).

When B and C are unrelated, the rule simplifies to:

  (A - B) ∪ C = (A ∪ C) - B"
  (elsa-type-debug ("(elsa-type-sum %s %s) elsa-diff-type elsa-type" this other)
    (let* ((pos (oref this positive))
           (neg (oref this negative)))
      (elsa-type-normalize
       (cond
        ((elsa-type-is-empty-p (elsa-type-intersect neg other))
         (elsa-diff-type :positive (elsa-type-sum pos other)
                         :negative neg))
        (t (let ((new-pos (elsa-type-sum pos other))
                 (new-neg (elsa-type-diff neg other)))
             (if (elsa-type-is-empty-p new-neg)
                 new-pos
               (elsa-type-diff new-pos new-neg)))))))))

(cl-defmethod elsa-type-sum ((this elsa-intersection-type) (other elsa-type))
  "(A ∩ B) ∪ C = (A ∪ C) ∩ (B ∪ C)"
  (elsa-type-debug ("(elsa-type-sum %s %s) elsa-intersection-type elsa-type" this other)
    (cond
     ((-all? #'elsa-function-type-p (oref this types))
      (elsa-sum-type :types (list this other)))
     (t (elsa-type-normalize
         (-reduce
          #'elsa-type-intersect
          (-map (lambda (type) (elsa-type-sum type other)) (oref this types))))))))

(cl-defmethod elsa-type-diff ((this elsa-type) other)
  "Any base type without another is the same type, there is no intersection."
  (elsa-type-debug ("(elsa-type-diff %s %s) elsa-type t" this other)
    (elsa-type-normalize
     (cond
      ;; If this is contained in other, we are left with nothing.
      ((elsa-type-accept other this)
       (elsa-type-empty))
      ((and (elsa-const-type-p other)
            (elsa-type-accept this (oref other type)))
       (elsa-diff-type :positive (clone this)
                       :negative (clone other)))
      ;; If the other is subset of this, there is no way to represent
      ;; this other than a raw difference.
      ((elsa-type-accept this other)
       (elsa-diff-type :positive (clone this)
                       :negative (clone other)))
      (t
       (clone this))))))

(cl-defmethod elsa-type-diff ((_this elsa-type-mixed) other)
  "Mixed is one with everything, so we need to subtract OTHER from the world.

However, mixed without (Mixed without something) is something.

This uses the rule that A - (A - B) = A ∩ B = B where A is
everything (Mixed)."
  (elsa-type-debug ("(elsa-type-diff %s %s) elsa-type-mixed t" _this other)
    (cond
     ((elsa-diff-type-p other)
      (let ((pos (oref other positive))
            (neg (oref other negative)))
        (cond
         ((elsa-type-mixed-p pos)
          (clone neg))
         (t
          (elsa-type-normalize
           (elsa-sum-type :types (list (elsa-type-normalize
                                        (elsa-diff-type :negative (clone pos)))
                                       (clone neg))))))))
     (t
      (elsa-type-normalize (elsa-diff-type :negative (clone other)))))))

(cl-defmethod elsa-type-diff ((this elsa-type-number) (other elsa-type-number))
  "Numbers are handled in special way, see `elsa-type-number'."
  (elsa-type-debug ("(elsa-type-diff %s %s) elsa-type-number elsa-type-number" this other)
    (cond
     ((and (elsa-type-number-p this) (elsa-type-int-p other))
      (elsa-type-float))
     ((and (elsa-type-number-p this) (elsa-type-float-p other))
      (elsa-type-int))
     ((and (elsa-type-int-p this) (elsa-type-float-p other))
      (elsa-type-int))
     (t (cl-call-next-method)))))

(cl-defmethod elsa-type-diff ((this elsa-type-bool) (other elsa-type-bool))
  "Bool without T is Nil."
  (elsa-type-debug ("(elsa-type-diff %s %s) elsa-type-bool elsa-type-bool" this other)
    (cond
     ((and (elsa-type-t-p this) (elsa-type-nil-p other))
      (elsa-type-t))
     ((and (elsa-type-nil-p this) (elsa-type-t-p other))
      (elsa-type-nil))
     ((and (elsa-type-bool-p this) (elsa-type-t-p other))
      (elsa-type-nil))
     ((and (elsa-type-bool-p this) (elsa-type-nil-p other))
      (elsa-type-t))
     (t (cl-call-next-method)))))

(cl-defmethod elsa-type-diff ((this elsa-type) (other elsa-sum-type))
  "A difference of a type and sum is THIS minus all the summed types."
  (elsa-type-debug ("(elsa-type-diff %s %s) elsa-type elsa-sum-type" this other)
    (let ((new (clone this)))
      (-reduce-from 'elsa-type-diff new (oref other types)))))

;; TODO: what about (or int string) - (const "foo").  This should
;; probably become (or int (diff string (const "foo")))
(cl-defmethod elsa-type-diff ((this elsa-sum-type) (other elsa-type))
  "(A ∪ B) - C = (A - C) ∪ (B - C)."
  (elsa-type-debug ("(elsa-type-diff %s %s) elsa-sum-type elsa-type" this other)
    (elsa-type-normalize
     (cond
      ((not (elsa-type-composite-p other))
       (let ((pos (-map
                   (lambda (type) (elsa-type-diff type other))
                   (oref this types))))
         (elsa-diff-type
          :positive (elsa-type-normalize (elsa-sum-type :types pos))
          :negative other)))
      (t (-reduce
          #'elsa-type-sum
          (-map (lambda (type) (elsa-type-diff type other)) (oref this types))))))))

(cl-defmethod elsa-type-diff ((this elsa-type) (other elsa-diff-type))
  "This uses the rule that A - (B - C) = (A - B) ∪ (A ∩ B ∩ C)"
  (elsa-type-debug ("(elsa-type-diff %s %s) elsa-type elsa-diff-type" this other)
    (if (not (elsa-type-composite-p this))
        (elsa-type-normalize
         (elsa-sum-type :types (list (elsa-type-diff (clone this)
                                                     (clone (oref other positive)))
                                     (elsa-type-intersect-all
                                      (list this (oref other positive) (oref other negative))))))
      (cl-call-next-method))))

(cl-defmethod elsa-type-diff ((this elsa-diff-type) other)
  "This uses the rule that (A - B) - C = A - (B ∪ C)"
  (elsa-type-debug ("(elsa-type-diff %s %s) elsa-diff-type t" this other)
    (elsa-type-normalize
     (elsa-diff-type :positive (clone (oref this positive))
                     :negative (elsa-type-sum (oref this negative) other)))))

(cl-defmethod elsa-type-intersect ((this elsa-type) (other elsa-type))
  "Basic primitive type intersection."
  (elsa-type-debug ("(elsa-type-intersect %s %s)" this other)
    (cond
     ((elsa-readonly-type-p this)
      (elsa-type-normalize
       (elsa-readonly-type :type (elsa-type-intersect (oref this type) other))))
     ((elsa-type-composite-p other)
      (elsa-type-intersect other this))
     ((and (elsa-type-list-p this)
           (or (elsa-type-bool-p other) (elsa-type-nil-p other)))
      (elsa-type-nil))
     ((and (or (elsa-type-bool-p this) (elsa-type-nil-p this))
           (elsa-type-list-p other))
      (elsa-type-nil))
     ((elsa-type-accept this other)
      (clone other))
     ((elsa-type-accept other this)
      (clone this))
     (t (elsa-type-empty)))))

(cl-defmethod elsa-type-intersect ((this elsa-function-type) (other elsa-function-type))
  "An intersection of functions represents overloads.

A function can have multiple signatures and it is all of them at
the same time.  For example, the same function can process
numbers or strings and return some (different) return values
based on the input type.

It is not correct to say that the function is one *or* the other,
because that only implies we are not certain which implementation
it is, but once determined it collapses to only one possible
signature.  A function with overloads is always all of the
possible signatures."
  (elsa-type-debug ("(elsa-type-intersect %s %s)" this other)
    (if (elsa-type-equivalent-p this other)
        (clone this)
      (elsa-intersection-type :types (list this other)))))

(cl-defmethod elsa-type-intersect ((this elsa-type-number) (other elsa-type-number))
  "Float and ints intersect to empty, other types use usual definition."
  (elsa-type-debug ("(elsa-type-intersect %s %s)" this other)
    (cond
     ((and (elsa-type-float-p this) (elsa-type-int-p other))
      (elsa-type-empty))
     ((and (elsa-type-int-p this) (elsa-type-float-p other))
      (elsa-type-empty))
     (t (cl-call-next-method)))))

(cl-defmethod elsa-type-intersect ((this elsa-sum-type) (other elsa-type))
  "(A ∪ B) ∩ C = (A ∩ C) ∪ (B ∩ C)"
  (elsa-type-debug ("(elsa-type-intersect %s %s)" this other)
    (elsa-type-normalize
     (-reduce
      #'elsa-type-sum
      (-map (lambda (type) (elsa-type-intersect type other)) (oref this types))))))

(cl-defmethod elsa-type-intersect ((this elsa-sum-type) (other elsa-function-type))
  "Intersection of sum and function.

This is not recursively resolved because it leads to infinite
recursive call between sum and intersect."
  (elsa-type-debug ("(elsa-type-intersect %s %s)" this other)
    (elsa-type-normalize
     (elsa-sum-type :types
       (-map (lambda (type) (elsa-type-intersect type other))
             (oref this types))))))

(cl-defmethod elsa-type-intersect ((this elsa-diff-type) (other elsa-type))
  "(B - A) ∩ C = (B ∩ C) - A"
  (elsa-type-debug ("(elsa-type-intersect %s %s)" this other)
    (let ((pos (oref this positive))
          (neg (oref this negative)))
      (elsa-type-normalize
       (elsa-type-diff (elsa-type-intersect pos other) (clone neg))))))

(cl-defmethod elsa-type-intersect ((this elsa-intersection-type) (other elsa-type))
  "(A ∩ B) ∩ C = (A ∩ C) ∩ (B ∩ C)"
  (elsa-type-debug ("(elsa-type-intersect %s %s)" this other)
    (elsa-type-normalize
     (-reduce
      #'elsa-type-intersect
      (-map (lambda (type) (elsa-type-intersect type other)) (oref this types))))))

(cl-defmethod elsa-type-intersect ((this elsa-intersection-type) (other elsa-function-type))
  "(A ∩ B) ∩ C = (A ∩ C) ∩ (B ∩ C)"
  (elsa-type-debug ("(elsa-type-intersect %s %s)" this other)
    (elsa-type-normalize
     (elsa-intersection-type :types (append (oref this types) (list other))))))

(cl-defmethod elsa-type-intersect ((this elsa-intersection-type) (other elsa-intersection-type))
  "(A ∩ B ∩ ...) ∩ (C ∩ D ∩ ...) = A ∩ B ∩ C ∩ D ∩ ...

Intersection of two intersections in intersection of all types."
  (elsa-type-debug ("(elsa-type-intersect %s %s)" this other)
    (elsa-type-normalize
     ;; TODO: just as with sum, here we need to filter out repeated types.
     (elsa-intersection-type :types (-concat (oref this types) (oref other types))))))

(provide 'elsa-type-algebra)
;;; elsa-type-algebra.el ends here
