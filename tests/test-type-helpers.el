;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa-type-helpers)

(defmacro elsa-test-describe-type (&rest spec)
  `(elsa-type-describe (elsa-make-type ,@spec)))

(describe "Elsa type helpers"

  (describe "elsa-make-type"

    (it "should make a primitive type"
      (expect (elsa-test-describe-type int) :to-equal "int"))

    (it "should make a primitive type wrapped in superfluous parens"
      (expect (elsa-test-describe-type (int)) :to-equal "int"))

    (it "should make a primitive type wrapped in superfluous parens multiple times"
      (expect (elsa-test-describe-type ((int))) :to-equal "int"))

    (it "should make a simple function type"
      (expect (elsa-test-describe-type (function (int) int)) :to-equal "(function (int) int)"))

    (it "should make a simple function type wrapped in superfluous parens"
      (expect (elsa-test-describe-type ((function (int) (Int)))) :to-equal "(function (int) int)"))

    (it "should create a variadic type"
      (expect (elsa-test-describe-type (function (&rest int) int)) :to-equal
              "(function (&rest int) int)"))

    (it "should"
      (expect (elsa-test-describe-type (function (int int) bool)) :to-equal
              "(function (int int) bool)"))

    (it "should"
      (expect (elsa-test-describe-type (function (int) (list int))) :to-equal
              "(function (int) (list int))"))

    (it "should"
      (expect (elsa-test-describe-type (function (int) (list int))) :to-equal
              "(function (int) (list int))"))

    (it "should"
      (expect (elsa-test-describe-type (function (int) (cons string nil))) :to-equal
              "(function (int) (cons string nil))"))

    (it "should"
      (expect (elsa-test-describe-type (or int bool)) :to-equal
              "(or int bool)"))

    (it "should"
      (expect (elsa-test-describe-type (function ((or int bool)) float)) :to-equal
              "(function ((or int bool)) float)"))

    (it "should"
      (expect (elsa-test-describe-type (or int (function (bool) float))) :to-equal
              "(or int (function (bool) float))"))

    (it "should"
      (expect (elsa-test-describe-type (or int (function (bool) (or float string)))) :to-equal
              "(or int (function (bool) (or float string)))"))

    (it "should"
      (expect (elsa-test-describe-type (function (or int bool) (cons (or int string) buffer))) :to-equal
              "(function (int bool) (cons (or int string) buffer))"))

    (it "should"
      (expect (elsa-test-describe-type (function (int (function (string) float) buffer) (cons (or int string) buffer))) :to-equal
              "(function (int (function (string) float) buffer) (cons (or int string) buffer))"))

    (it "should support construction of lists through vector shorthand"
      (expect (elsa-test-describe-type (list int)) :to-equal "(list int)"))

    (it "should support construction of lists of lists through vector shorthand"
      (expect (elsa-test-describe-type (list (list int))) :to-equal "(list (list int))"))

    (it "should support construction of lists of functions through vector shorthand"
      (expect (elsa-test-describe-type (list (function (int) bool))) :to-equal "(list (function (int) bool))"))

    (it "should support construction of lists of sums through vector shorthand"
      (expect (elsa-test-describe-type (list (or int bool))) :to-equal "(list (or int bool))"))

    (it "should support construction of lists of higher order types through vector shorthand"
      (expect (elsa-test-describe-type (list (cons int string))) :to-equal
              "(list (cons int string))")))

  (describe "elsa-type-sum"

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

    (it "should do nothing when adding a non-nullable type already present as nullable"
      (let ((sum (elsa-type-sum (elsa-make-type (or int nil)) (elsa-make-type int))))
        (expect (length (oref sum types)) :to-equal 2)
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should be made nullable and then do nothing when adding a nullable type when non-nullable is already present"
      (let ((sum (elsa-type-sum (elsa-make-type int) (elsa-make-type (or int nil)))))
        (expect (length (oref sum types)) :to-equal 2)
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should return original type if summed with itself"
      (let ((sum (elsa-type-sum (elsa-make-type int) (elsa-make-type int))))
        (expect (elsa-type-int-p sum) :to-be-truthy)))

    (it "should not add the same primitive type twice"
      (let ((sum (elsa-type-sum (elsa-make-type float) (elsa-make-type int))))
        (setq sum (elsa-type-sum sum (elsa-make-type int)))
        (expect (length (oref sum types)) :to-equal 2))))


  (describe "elsa-type-diff"


    (describe "primitive"


      (it "string diffed with itself should give empty"
        (expect (elsa-type-diff (elsa-make-type string) (elsa-make-type string))
                :to-equal (elsa-make-type [])))

      (it "symbol diffed with itself should give empty"
        (expect (elsa-type-diff (elsa-make-type symbol) (elsa-make-type symbol))
                :to-equal (elsa-make-type []))))

    (describe "number"


      (it "diffing int with int should give empty"
        (expect (elsa-type-diff (elsa-make-type int) (elsa-make-type int))
                :to-equal (elsa-make-type [])))

      (it "diffing float with float should give empty"
        (expect (elsa-type-diff (elsa-make-type float) (elsa-make-type float))
                :to-equal (elsa-make-type [])))

      (it "diffing number with int should give float"
        (expect (elsa-type-diff (elsa-make-type number) (elsa-make-type int))
                :to-equal (elsa-make-type float)))

      (it "diffing number with float should give int"
        (expect (elsa-type-diff (elsa-make-type number) (elsa-make-type float))
                :to-equal (elsa-make-type int)))

      (it "diffing int with float should give int"
        (expect (elsa-type-diff (elsa-make-type int) (elsa-make-type float))
                :to-equal (elsa-make-type int)))

      (it "diffing float with int should give float"
        (expect (elsa-type-diff (elsa-make-type float) (elsa-make-type int))
                :to-equal (elsa-make-type float))))


    (describe "sum"


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
                  :to-equal (elsa-make-type float)))))


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

  (describe "elsa-type-is-nil"

    (it "should return true₃ if type is definitely nil"
      (expect (elsa-type-is-nil (elsa-type-nil))
              :to-equal (trinary-true)))

    (it "should return maybe₃ if type can be nil"
      (expect (elsa-type-is-nil (elsa-make-type (or string nil)))
              :to-equal (trinary-maybe)))

    (it "should return false₃ if type is definitely not nil"
      (expect (elsa-type-is-nil (elsa-type-string))
              :to-equal (trinary-false))))

  (describe "elsa-type-is-non-nil"

    (it "should return true₃ if type is definitely non-nil"
      (expect (elsa-type-is-non-nil (elsa-type-string))
              :to-equal (trinary-true)))

    (it "should return maybe₃ if type can be nil"
      (expect (elsa-type-is-non-nil (elsa-make-type (or string nil)))
              :to-equal (trinary-maybe)))

    (it "should return false₃ if type is definitely nil"
      (expect (elsa-type-is-non-nil (elsa-type-nil))
              :to-equal (trinary-false)))))
