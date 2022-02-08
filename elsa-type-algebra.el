;;; elsa-type-algebra.el --- Elsa type algebra -*- lexical-binding: t -*-

(require 'eieio)
(require 'elsa-types)

(cl-defgeneric elsa-type-sum (this other)
  "Return the sum of THIS and OTHER type.

A sum accept anything that either THIS or OTHER accepts.")

(cl-defmethod elsa-type-sum ((this elsa-type) (other elsa-type))
  "Basic primitive type sum."
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
   (t (let ((sum (elsa-sum-type :types (list this))))
        (elsa-type-sum sum other)))))

(cl-defmethod elsa-type-sum ((this elsa-readonly-type) (other elsa-readonly-type))
  (elsa-readonly-type :type (elsa-type-sum (oref this type) (oref other type))))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-sum-type))
  "(A ∪ B ∪ ...) ∪ (C ∪ D ∪ ...) = A ∪ B ∪ C ∪ D ∪ ...

This is only for performance reasons, because we know we are
combining two unions, we don't have to unpack each argument of
first union against each of the second."
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
     (elsa-sum-type :types (-concat (nreverse new-this) (nreverse new-other))))))

(cl-defmethod elsa-type-sum ((this elsa-sum-type) (other elsa-type))
  "(A ∪ B) ∪ C = (A ∪ C) ∪ (B ∪ C)"
  (elsa-type-normalize
   (if (elsa-type-composite-p other)
       (-reduce
        #'elsa-type-sum
        (-map (lambda (type) (elsa-type-sum type other)) (oref this types)))
     (if (elsa-type-accept this other)
         (clone this)
       (elsa-type-sum this (elsa-sum-type :types (list other)))))))

(cl-defmethod elsa-type-sum ((this elsa-diff-type) (other elsa-type))
  "(A \ B) ∪ C = (A ∪ C) \ (B \ C).

When B and C are unrelated, the rule simplifies to:

  (A \ B) ∪ C = (A ∪ C) \ (B)"
  (let* ((pos (oref this positive))
         (neg (oref this negative)))
    (elsa-type-normalize
     (cond
      ((elsa-type-equivalent-p
        (elsa-type-empty)
        (elsa-type-intersect neg other))
       (elsa-diff-type :positive (elsa-type-sum pos other)
                       :negative neg))
      (t (elsa-type-diff
          (elsa-type-sum pos other)
          (elsa-type-diff neg other)))))))

(cl-defmethod elsa-type-sum ((this elsa-intersection-type) (other elsa-type))
  "(A ∩ B) ∪ C = (A ∪ C) ∩ (B ∪ C)"
  (elsa-type-normalize
   (-reduce
    #'elsa-type-intersect
    (-map (lambda (type) (elsa-type-sum type other)) (oref this types)))))

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

(cl-defmethod elsa-type-diff ((this elsa-type-number) (other elsa-type-number))
  "Numbers are handled in special way, see `elsa-type-int'."
  (cond
   ((and (elsa-type-number-p this) (elsa-type-int-p other))
    (elsa-type-float))
   ((and (elsa-type-number-p this) (elsa-type-float-p other))
    (elsa-type-int))
   (t (cl-call-next-method this other))))

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

;; TODO: what about (or int string) - (const "foo").  This should
;; probably become (or int (diff string (const "foo")))
(cl-defmethod elsa-type-diff ((this elsa-sum-type) (other elsa-type))
  "(A ∪ B) \ C = (A \ C) ∪ (B \ C)."
  (elsa-type-normalize
   (-reduce
    #'elsa-type-sum
    (-map (lambda (type) (elsa-type-diff type other)) (oref this types))))
  ;; (let (new)
  ;;   (-each (oref this types)
  ;;     (lambda (type)
  ;;       (let ((diff-type (elsa-type-diff type other)))
  ;;         (unless (elsa-type-empty-p diff-type)
  ;;           (push (clone diff-type) new)))))
  ;;   (elsa-type-normalize
  ;;    (elsa-sum-type :types (nreverse new))))
  )

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
  "Basic primitive type intersection."
  (cond
   ((elsa-type-composite-p other)
    (elsa-type-intersect other this))
   ((elsa-type-accept this other)
    (clone other))
   ((elsa-type-accept other this)
    (clone this))
   (t (elsa-type-empty))))

(cl-defmethod elsa-type-intersect ((this elsa-sum-type) (other elsa-type))
  "(A ∪ B) ∩ C = (A ∩ C) ∪ (B ∩ C)"
  (elsa-type-normalize
   (-reduce
    #'elsa-type-sum
    (-map (lambda (type) (elsa-type-intersect type other)) (oref this types)))))

(cl-defmethod elsa-type-intersect ((this elsa-diff-type) (other elsa-type))
  "(B \ A) ∩ C = (B ∩ C) \ A"
  (let ((pos (oref this positive))
        (neg (oref this negative)))
    (elsa-type-normalize
     (elsa-type-diff (elsa-type-intersect pos other) (clone neg)))))

(cl-defmethod elsa-type-intersect ((this elsa-intersection-type) (other elsa-type))
  "(A ∩ B) ∩ C = (A ∩ C) ∩ (B ∩ C)"
  (elsa-type-normalize
   (-reduce
    #'elsa-type-intersect
    (-map (lambda (type) (elsa-type-intersect type other)) (oref this types)))))

(provide 'elsa-type-algebra)
;;; elsa-type-algebra.el ends here
