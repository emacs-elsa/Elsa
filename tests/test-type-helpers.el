;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa-type-helpers)

(defmacro elsa-test-describe-type (&rest spec)
  `(elsa-type-describe (elsa-make-type ,@spec)))

(describe "Elsa type helpers"

  (describe "elsa-make-type"

    (it "should make a primitive type"
      (expect (elsa-test-describe-type Int) :to-equal "int"))

    (it "should make a primitive type wrapped in superfluous parens"
      (expect (elsa-test-describe-type (Int)) :to-equal "int"))

    (it "should make a primitive type wrapped in superfluous parens multiple times"
      (expect (elsa-test-describe-type ((Int))) :to-equal "int"))

    (it "should make a simple function type"
      (expect (elsa-test-describe-type Int -> Int) :to-equal "(function (int) int)"))

    (it "should make a simple function type wrapped in superfluous parens"
      (expect (elsa-test-describe-type (Int -> Int)) :to-equal "(function (int) int)"))

    (it "should create a variadic type"
      (expect (elsa-test-describe-type Int... -> Int) :to-equal
              "(function (&rest int) int)"))

    (it "should"
      (expect (elsa-test-describe-type Int -> Int -> Bool) :to-equal
              "(function (int int) bool)"))

    (it "should"
      (expect (elsa-test-describe-type Int -> List Int) :to-equal
              "(function (int) (list int))"))

    (it "should"
      (expect (elsa-test-describe-type Int -> (List Int)) :to-equal
              "(function (int) (list int))"))

    (it "should"
      (expect (elsa-test-describe-type Int -> Cons String Nil) :to-equal
              "(function (int) (cons string nil))"))

    (it "should"
      (expect (elsa-test-describe-type Int | Bool) :to-equal
              "(or int bool)"))

    (it "should"
      (expect (elsa-test-describe-type Int | Bool -> Float) :to-equal
              "(function ((or int bool)) float)"))

    (it "should"
      (expect (elsa-test-describe-type Int | (Bool -> Float)) :to-equal
              "(or int (function (bool) float))"))

    (it "should"
      (expect (elsa-test-describe-type Int | (Bool -> Float | String)) :to-equal
              "(or int (function (bool) (or float string)))"))

    (it "should"
      (expect (elsa-test-describe-type Int -> Bool -> Cons (Int | String) Buffer) :to-equal
              "(function (int bool) (cons (or int string) buffer))"))

    (it "should"
      (expect (elsa-test-describe-type Int -> (String -> Float) -> Buffer -> Cons (Int | String) Buffer) :to-equal
              "(function (int (function (string) float) buffer) (cons (or int string) buffer))"))

    (it "should support construction of lists through vector shorthand"
      (expect (elsa-test-describe-type [Int]) :to-equal "(list int)"))

    (it "should support construction of lists of lists through vector shorthand"
      (expect (elsa-test-describe-type [[Int]]) :to-equal "(list (list int))"))

    (it "should support construction of lists of functions through vector shorthand"
      (expect (elsa-test-describe-type [Int -> Bool]) :to-equal "(list (function (int) bool))"))

    (it "should support construction of lists of sums through vector shorthand"
      (expect (elsa-test-describe-type [Int | Bool]) :to-equal "(list (or int bool))"))

    (it "should support construction of lists of higher order types through vector shorthand"
      (expect (elsa-test-describe-type [Cons Int String]) :to-equal
              "(list (cons int string))")))

  (describe "elsa-type-sum"

    (it "should not create sum type if other regular type is already accepted by this"
      (let ((number (elsa-make-type Number))
            (int (elsa-make-type Int)))
        (expect (eieio-object-class (elsa-type-sum number int))
                :to-be 'elsa-type-number)))


    (it "should not create sum type if other sum type is already accepted by this"
      (let ((number (elsa-make-type Number))
            (int-or-float (elsa-make-type Int | Float)))
        (expect (eieio-object-class (elsa-type-sum number int-or-float))
                :to-be 'elsa-type-number)))

    (it "should do nothing when adding a non-nullable type already present as nullable"
      (let ((sum (elsa-type-sum (elsa-make-type Int?) (elsa-make-type Int))))
        (expect (length (oref sum types)) :to-equal 2)
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should be made nullable and then do nothing when adding a nullable type when non-nullable is already present"
      (let ((sum (elsa-type-sum (elsa-make-type Int) (elsa-make-type Int?))))
        (expect (length (oref sum types)) :to-equal 2)
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should return original type if summed with itself"
      (let ((sum (elsa-type-sum (elsa-make-type Int) (elsa-make-type Int))))
        (expect (elsa-type-int-p sum) :to-be-truthy)))

    (it "should not add the same primitive type twice"
      (let ((sum (elsa-type-sum (elsa-make-type Float) (elsa-make-type Int))))
        (setq sum (elsa-type-sum sum (elsa-make-type Int)))
        (expect (length (oref sum types)) :to-equal 2))))


  (describe "elsa-type-diff"


    (describe "primitive"


      (it "string diffed with itself should give empty"
        (expect (elsa-type-diff (elsa-make-type String) (elsa-make-type String))
                :to-equal (elsa-make-type Empty)))

      (it "symbol diffed with itself should give empty"
        (expect (elsa-type-diff (elsa-make-type Symbol) (elsa-make-type Symbol))
                :to-equal (elsa-make-type Empty))))

    (describe "number"


      (it "diffing int with int should give empty"
        (expect (elsa-type-diff (elsa-make-type Int) (elsa-make-type Int))
                :to-equal (elsa-make-type Empty)))

      (it "diffing float with float should give empty"
        (expect (elsa-type-diff (elsa-make-type Float) (elsa-make-type Float))
                :to-equal (elsa-make-type Empty)))

      (it "diffing number with int should give float"
        (expect (elsa-type-diff (elsa-make-type Number) (elsa-make-type Int))
                :to-equal (elsa-make-type Float)))

      (it "diffing number with float should give int"
        (expect (elsa-type-diff (elsa-make-type Number) (elsa-make-type Float))
                :to-equal (elsa-make-type Int)))

      (it "diffing int with float should give int"
        (expect (elsa-type-diff (elsa-make-type Int) (elsa-make-type Float))
                :to-equal (elsa-make-type Int)))

      (it "diffing float with int should give float"
        (expect (elsa-type-diff (elsa-make-type Float) (elsa-make-type Int))
                :to-equal (elsa-make-type Float))))


    (describe "sum"


      (describe "subtracting from sum"


        (it "should remove primitive type from a sum"
          (expect (elsa-type-diff
                   (elsa-sum-type
                    :types (list (elsa-make-type Int)
                                 (elsa-make-type String)))
                   (elsa-make-type Int))
                  :to-equal (elsa-make-type String)))

        (it "should reduce number in a sum to float if int is subtracted"
          (expect (elsa-type-equivalent-p
                   (elsa-type-diff
                    (elsa-sum-type
                     :types (list (elsa-make-type Number)
                                  (elsa-make-type String)))
                    (elsa-make-type Int))
                   (elsa-make-type String | Float))
                  :to-be t)))


      (describe "subtracting a sum"


        (it "should empty a primitive if the sum contains it"
          (expect (elsa-type-diff
                   (elsa-make-type Int)
                   (elsa-sum-type
                    :types (list (elsa-make-type Int)
                                 (elsa-make-type String))))
                  :to-equal (elsa-make-type Empty)))

        (it "should resolve a number to float if the sum contains int"
          (expect (elsa-type-diff
                   (elsa-make-type Number)
                   (elsa-sum-type
                    :types (list (elsa-make-type Int)
                                 (elsa-make-type String))))
                  :to-equal (elsa-make-type Float)))))


    (describe "mixed"


      (it "without string should be mixed \ string"
        (expect (oref (elsa-type-diff
                       (elsa-make-type Mixed)
                       (elsa-make-type String)) negative)
                :to-equal (elsa-make-type String)))

      (it "without string and int should be mixed \ (string | int)"
        (expect (elsa-type-equivalent-p
                 (oref (elsa-type-diff
                        (elsa-make-type Mixed)
                        (elsa-make-type String | Int)) negative)
                 (elsa-make-type String | Int))
                :to-be t))

      (it "without string and int subtracted iteratively should be mixed \ (string | int)"
        (expect (elsa-type-equivalent-p
                 (oref (elsa-type-diff
                        (elsa-type-diff
                         (elsa-make-type Mixed)
                         (elsa-make-type String))
                        (elsa-make-type Int))
                       negative)
                 (elsa-make-type String | Int))
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
                 (elsa-make-type Number | String)
                 (elsa-type-int))
                :to-be-type-equivalent
          (elsa-make-type Int)))))

  (describe "elsa-type-is-nil"

    (it "should return true₃ if type is definitely nil"
      (expect (elsa-type-is-nil (elsa-type-nil))
              :to-equal (trinary-true)))

    (it "should return maybe₃ if type can be nil"
      (expect (elsa-type-is-nil (elsa-make-type String?))
              :to-equal (trinary-maybe)))

    (it "should return false₃ if type is definitely not nil"
      (expect (elsa-type-is-nil (elsa-type-string))
              :to-equal (trinary-false))))

  (describe "elsa-type-is-non-nil"

    (it "should return true₃ if type is definitely non-nil"
      (expect (elsa-type-is-non-nil (elsa-type-string))
              :to-equal (trinary-true)))

    (it "should return maybe₃ if type can be nil"
      (expect (elsa-type-is-non-nil (elsa-make-type String?))
              :to-equal (trinary-maybe)))

    (it "should return false₃ if type is definitely nil"
      (expect (elsa-type-is-non-nil (elsa-type-nil))
              :to-equal (trinary-false)))))
