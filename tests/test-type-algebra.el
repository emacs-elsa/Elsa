;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-types)
(require 'elsa-type-helpers)

(defmacro elsa-test-describe-type (&rest spec)
  `(elsa-type-describe (elsa-make-type ,@spec)))

(describe "Elsa type algebra"

  (describe "elsa-type-sum"

    (describe "with empty"

      (it "should be a left-identity to sum with empty"
        (expect (elsa-type-describe
                 (elsa-type-sum (elsa-type-int) (elsa-type-empty)))
                :to-equal "int"))

      (it "should be a right-identity to sum with empty"
        (expect (elsa-type-describe
                 (elsa-type-sum (elsa-type-empty) (elsa-type-int)))
                :to-equal "int")))

    (describe "first arg primitive"

      (it "should not create sum type if other regular type is already accepted by this"
        (let ((number (elsa-make-type number))
              (int (elsa-make-type int)))
          (expect (eieio-object-class (elsa-type-sum number int))
                  :to-be 'elsa-type-number)))

      (it "should not create sum type if other sum type is already accepted by this"
        (let ((number (elsa-make-type number))
              (int-or-float (elsa-make-type (or int float))))
          (expect (eieio-object-class (elsa-type-sum number int-or-float))
                  :to-be 'elsa-type-number)))

      (it "should be made nullable and then do nothing when adding a nullable type when non-nullable is already present"
        (let ((sum (elsa-type-sum (elsa-make-type int)
                                  (elsa-make-type (or int nil)))))
          (expect (length (oref sum types)) :to-equal 2)
          (expect (elsa-type-nullable-p sum) :to-be-truthy)))

      (it "should return original type if summed with itself"
        (let ((sum (elsa-type-sum (elsa-make-type int) (elsa-make-type int))))
          (expect (elsa-type-int-p sum) :to-be-truthy)))

      (it "should not add the same primitive type twice"
        (let ((sum (elsa-type-sum (elsa-make-type string) (elsa-make-type int))))
          (setq sum (elsa-type-sum sum (elsa-make-type int)))
          (expect (length (oref sum types)) :to-equal 2)))

      (it "should absorb a const type of the same type"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type int)
                  (elsa-make-type (or string (const 1)))))
                :to-equal "(or string int)")))

    (describe "first arg sum type"

      (it "should do nothing when adding a non-nullable type already present as nullable"
        (let ((sum (elsa-type-sum (elsa-make-type (or int nil))
                                  (elsa-make-type int))))
          (expect (length (oref sum types)) :to-equal 2)
          (expect (elsa-type-nullable-p sum) :to-be-truthy))))


    (describe "constants"

      (it "should simplify sum of constant with itself to itself"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (const 1))
                  (elsa-make-type (const 1))))
                :to-equal "(const 1)"))

      (it "should sum two constants"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (const 1))
                  (elsa-make-type (const 2))))
                :to-equal "(or (const 1) (const 2))")))

    (describe "first arg diff type"

      (it "should complement diffed positive type if the same type is added"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (diff int (const 1)))
                  (elsa-make-type int)))
                :to-equal "int"))

      (it "should use bigger type if summed with a subtype"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (diff int (const 1)))
                  (elsa-make-type int)))
                :to-equal "int")

        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type int)
                  (elsa-make-type (diff int (const 1)))))
                :to-equal "int"))

      (it "sum of diff with the negative should yield positive"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (diff int (const 1)))
                  (elsa-make-type (const 1))))
                :to-equal "int")

        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (const 1))
                  (elsa-make-type (diff int (const 1)))))
                :to-equal "int"))

      (it "sum where other accepts negative should undo the diff type and return simpler type 1"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (diff int (const 1)))
                  (elsa-make-type (or string (const 1)))))
                :to-equal "(or string int)"))


      (it "sum where other accepts negative should undo the diff type and return simpler type 2"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (diff int (const 1)))
                  (elsa-make-type (or (const 1) (const 2)))))
                :to-equal "int"))

      (it "sum of diff type and another primitive type should sum the positives if there is no overlap of other with negative"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (diff int (const 1)))
                  (elsa-make-type string)))
                :to-equal "(diff (or int string) (const 1))"))

      (it "sum of diff types with same positive and no overlap in negatives should restore the positive type only"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (diff int (const 2)))
                  (elsa-make-type (diff int (const 1)))))
                :to-equal "int"))

      (it "sum of two mixed - int should stay the same"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (diff mixed int))
                  (elsa-make-type (diff mixed int))))
                :to-equal "(diff mixed int)"))

      (it "sum of two mixed with non-overlapping negatives should be mixed"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (diff mixed int))
                  (elsa-make-type (diff mixed string))))
                :to-equal "mixed")))

    (describe "intersections"

      (it "sum of two intersections which are logically empty should be empty"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-intersection-type
                   :types (list (elsa-make-type string) (elsa-make-type int)))
                  (elsa-intersection-type
                   :types (list (elsa-make-type symbol) (elsa-make-type float)))))
                :to-equal "empty"))

      (it "sum of anything with logically empty intersection should return the first argument unchanged"
        (expect (elsa-type-describe
                 (elsa-type-sum
                  (elsa-make-type (or int string))
                  (elsa-intersection-type
                   :types (list (elsa-make-type symbol) (elsa-make-type float)))))
                :to-equal "(or int string)"))))

  (describe "elsa-type-diff"

    (describe "primitive"

      (it "string diffed with itself should give empty"
        (expect (elsa-type-diff (elsa-make-type string) (elsa-make-type string))
                :to-equal (elsa-type-empty)))

      (it "symbol diffed with itself should give empty"
        (expect (elsa-type-diff (elsa-make-type symbol) (elsa-make-type symbol))
                :to-equal (elsa-type-empty)))

      (it "string diffed with symbol should return string unchanged as there is no domain overlap"
        (expect (elsa-type-diff (elsa-make-type string) (elsa-make-type symbol))
                :to-equal (elsa-make-type string)))

      (it "should return only positive if negative is not a subset of positive"
        (expect (elsa-type-describe
                 (elsa-type-diff
                  (elsa-make-type int)
                  (elsa-make-type (const "foo"))))
                :to-equal "int")))

    (describe "subtracting from sum"

      (it "should remove primitive type from a sum"
        (expect (elsa-type-diff
                 (elsa-sum-type
                  :types (list (elsa-make-type int)
                               (elsa-make-type string)))
                 (elsa-make-type int))
                :to-equal (elsa-make-type string)))

      (it "should reduce number in a sum to float if int is subtracted"
        (expect (elsa-type-equivalent-p
                 (elsa-type-diff
                  (elsa-sum-type
                   :types (list (elsa-make-type number)
                                (elsa-make-type string)))
                  (elsa-make-type int))
                 (elsa-make-type (or string float)))
                :to-be t))

      (it "should subtract a const which overlaps with some of the summands"
        (expect (elsa-type-describe
                 (elsa-type-diff
                  (elsa-make-type (or int string))
                  (elsa-make-type (const "foo"))))
                :to-equal "(diff (or string int) (const \"foo\"))")))

    (describe "subtracting a sum"

      (it "should empty a primitive if the sum contains it"
        (expect (elsa-type-diff
                 (elsa-make-type int)
                 (elsa-sum-type
                  :types (list (elsa-make-type int)
                               (elsa-make-type string))))
                :to-equal (elsa-type-empty)))

      (it "should resolve a number to float if the sum contains int"
        (expect (elsa-type-diff
                 (elsa-make-type number)
                 (elsa-sum-type
                  :types (list (elsa-make-type int)
                               (elsa-make-type string))))
                :to-equal (elsa-make-type float))))

    (describe "mixed"

      (it "without string should be mixed \ string"
        (expect (oref (elsa-type-diff
                       (elsa-make-type mixed)
                       (elsa-make-type string)) negative)
                :to-equal (elsa-make-type string)))

      (it "without string and int should be mixed \ (string | int)"
        (expect (elsa-type-equivalent-p
                 (oref (elsa-type-diff
                        (elsa-make-type mixed)
                        (elsa-make-type (or string int))) negative)
                 (elsa-make-type (or string int)))
                :to-be t))

      (it "without string and int subtracted iteratively should be mixed \ (string | int)"
        (expect (elsa-type-equivalent-p
                 (oref (elsa-type-diff
                        (elsa-type-diff
                         (elsa-make-type mixed)
                         (elsa-make-type string))
                        (elsa-make-type int))
                       negative)
                 (elsa-make-type (or string int)))
                :to-be t))))


  (describe "elsa-type-intersect"

    (describe "primitive types"

      (it "should resolve to empty for types with no intersection."
        (expect (elsa-type-intersect
                 (elsa-type-string)
                 (elsa-type-int))
                :to-be-type-equivalent (elsa-type-empty)))

      (it "should resolve to t if intersecting bool with t"
        (expect (elsa-type-intersect
                 (elsa-type-bool)
                 (elsa-type-t))
                :to-be-type-equivalent (elsa-type-t)))

      (it "should resolve to int if intersecting number with int"
        (expect (elsa-type-intersect
                 (elsa-type-number)
                 (elsa-type-int))
                :to-be-type-equivalent (elsa-type-int)))

      (it "should resolve to vector if intersecting sequence with vector"
        (expect (elsa-type-intersect
                 (elsa-type-sequence)
                 (elsa-type-vector))
                :to-be-type-equivalent (elsa-type-vector)))

      (it "should resolve to t if intersecting t with bool"
        (expect (elsa-type-intersect
                 (elsa-type-t)
                 (elsa-type-bool))
                :to-be-type-equivalent (elsa-type-t)))

      (it "should resolve to int if intersecting int with number"
        (expect (elsa-type-intersect
                 (elsa-type-int)
                 (elsa-type-number))
                :to-be-type-equivalent (elsa-type-int)))

      (it "should resolve to vector if intersecting vector with sequence"
        (expect (elsa-type-intersect
                 (elsa-type-vector)
                 (elsa-type-sequence))
                :to-be-type-equivalent (elsa-type-vector)))

      (it "intersection of two diff types with same positive should set negative to sum of negatives (with mixed)"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-make-type (diff mixed int))
                  (elsa-make-type (diff mixed string))))
                :to-equal "(diff mixed (or string int))"))

      (it "intersection of two diff types with same positive should set negative to sum of negatives (with const)"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-make-type (diff int (const 1)))
                  (elsa-make-type (diff int (const 2)))))
                :to-equal "(diff int (or (const 2) (const 1)))"))

      (it "should narrow primitive type to its subtype when intersected with a sum containing the subtype"
        (expect (elsa-type-intersect
                 (elsa-make-type number)
                 (elsa-make-type (or int string)))
                :to-be-type-equivalent (elsa-type-int))
        (expect (elsa-type-intersect
                 (elsa-make-type number)
                 (elsa-make-type (or int string)))
                :to-be-type-equivalent (elsa-type-int))))

    (describe "sum types"

      (it "should distribute the intersection over sum"
        (expect (elsa-type-intersect
                 (elsa-make-type (or number string))
                 (elsa-type-int))
                :to-be-type-equivalent
                (elsa-make-type int)))

      (it "should return common types over two sum types"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-sum-type :types (list (elsa-make-type symbol)
                                              (elsa-make-type int)
                                              (elsa-make-type string)))
                  (elsa-sum-type :types (list (elsa-make-type int)
                                              (elsa-make-type string)))))
                :to-equal "(or int string)"))

      (it "should return empty for two sum types with no common types"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-sum-type :types (list (elsa-make-type symbol)
                                              (elsa-make-type string)))
                  (elsa-sum-type :types (list (elsa-make-type int)))))
                :to-equal "empty")))

    (describe "diff types"

      (it "with its negative should give empty type"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-make-type (diff int (const 1)))
                  (elsa-make-type (const 1))))
                :to-equal "empty"))

      (it "with a constant overlapping the positive should give the constant"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-make-type (diff int (const 1)))
                  (elsa-make-type (const 2))))
                :to-equal "(const 2)")))

    (describe "constants"

      (it "should return empty for intersection of different constants"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-make-type (const 1))
                  (elsa-make-type (const 2))))
                :to-equal "empty"))

      (it "should simplify intersection of the same constant with itself to itself"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-make-type (const 1))
                  (elsa-make-type (const 1))))
                :to-equal "(const 1)")))

    (xdescribe "intersections"

      (it "intersection of intersections with no overlap should be empty"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-intersection-type
                   :types (list (elsa-make-type string) (elsa-make-type int)))
                  (elsa-intersection-type
                   :types (list (elsa-make-type symbol) (elsa-make-type float)))))
                :to-equal "empty"))

      (it "intersection of intersections one overlap should simplify to simple type"
        (expect (elsa-type-describe
                 (elsa-type-intersect
                  (elsa-intersection-type
                   :types (list (elsa-make-type number) (elsa-make-type int)))
                  (elsa-intersection-type
                   :types (list (elsa-make-type int) (elsa-make-type float)))))
                :to-equal "int")))))
