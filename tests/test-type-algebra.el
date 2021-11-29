;; -*- lexical-binding: t -*-

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
          (expect (length (oref sum types)) :to-equal 2))))

    (describe "first arg sum type"

      (it "should do nothing when adding a non-nullable type already present as nullable"
        (let ((sum (elsa-type-sum (elsa-make-type (or int nil))
                                  (elsa-make-type int))))
          (expect (length (oref sum types)) :to-equal 2)
          (expect (elsa-type-nullable-p sum) :to-be-truthy))))


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
                :to-equal "int"))))

  (describe "elsa-type-diff"

    (describe "primitive"

      (it "string diffed with itself should give empty"
        (expect (elsa-type-diff (elsa-make-type string) (elsa-make-type string))
                :to-equal (elsa-make-type [])))

      (it "symbol diffed with itself should give empty"
        (expect (elsa-type-diff (elsa-make-type symbol) (elsa-make-type symbol))
                :to-equal (elsa-make-type [])))

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
                :to-be t)))

    (describe "subtracting a sum"

      (it "should empty a primitive if the sum contains it"
        (expect (elsa-type-diff
                 (elsa-make-type int)
                 (elsa-sum-type
                  :types (list (elsa-make-type int)
                               (elsa-make-type string))))
                :to-equal (elsa-make-type [])))

      (it "should resolve a number to float if the sum contains int"
        (expect (elsa-type-diff
                 (elsa-make-type number)
                 (elsa-sum-type
                  :types (list (elsa-make-type int)
                               (elsa-make-type string))))
                :to-equal (elsa-make-type float))))

    (describe "number"

      (it "diffing int with int should give empty"
        (expect (elsa-type-describe
                 (elsa-type-diff (elsa-make-type int) (elsa-make-type int)))
                :to-equal "empty"))

      (it "diffing float with float should give empty"
        (expect (elsa-type-describe
                 (elsa-type-diff (elsa-make-type float) (elsa-make-type float)))
                :to-equal "empty"))

      (it "diffing number with int should give float"
        (expect (elsa-type-describe
                 (elsa-type-diff (elsa-make-type number) (elsa-make-type int)))
                :to-equal "float"))

      (it "diffing number with float should give int"
        (expect (elsa-type-describe
                 (elsa-type-diff (elsa-make-type number) (elsa-make-type float)))
                :to-equal "int"))

      (it "diffing int with float should give empty"
        (expect (elsa-type-describe
                 (elsa-type-diff (elsa-make-type int) (elsa-make-type float)))
                :to-equal "empty"))

      (it "diffing float with int should give (diff float int)"
        (expect (elsa-type-describe
                 (elsa-type-diff (elsa-make-type float) (elsa-make-type int)))
                :to-equal "float")))

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
                :to-be-type-equivalent (elsa-type-vector))))

    (describe "sum types"

      (it "should distribute the intersection over sum"
        (expect (elsa-type-intersect
                 (elsa-make-type (or number string))
                 (elsa-type-int))
                :to-be-type-equivalent
                (elsa-make-type int)))))
  )
