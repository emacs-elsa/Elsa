;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa-type-helpers)

(describe "Elsa type helpers"

  (describe "elsa-make-type"

    (it "should"
      (expect (elsa-make-type Int) :to-equal
              [eieio-class-tag--elsa-type-int]))

    (it "should"
      (expect (elsa-make-type (Int)) :to-equal
              [eieio-class-tag--elsa-type-int]))

    (it "should"
      (expect (elsa-make-type ((Int))) :to-equal
              [eieio-class-tag--elsa-type-int]))

    (it "should"
      (expect (elsa-make-type Int -> Int) :to-equal
              [eieio-class-tag--elsa-function-type
               ([eieio-class-tag--elsa-type-int])
               [eieio-class-tag--elsa-type-int]]))

    (it "should"
      (expect (elsa-make-type (Int -> Int)) :to-equal
              [eieio-class-tag--elsa-function-type
               ([eieio-class-tag--elsa-type-int])
               [eieio-class-tag--elsa-type-int]]))

    (it "should"
      (expect (elsa-make-type Int -> Int -> Bool) :to-equal
              [eieio-class-tag--elsa-function-type
               ([eieio-class-tag--elsa-type-int]
                [eieio-class-tag--elsa-type-int])
               [eieio-class-tag--elsa-type-bool]]))

    (it "should"
      (expect (elsa-make-type Int -> List Int) :to-equal
              [eieio-class-tag--elsa-function-type
               ([eieio-class-tag--elsa-type-int])
               [eieio-class-tag--elsa-type-list
                [eieio-class-tag--elsa-type-int]
                [eieio-class-tag--elsa-type-int]
                [eieio-class-tag--elsa-type-int]]]))

    (it "should"
      (expect (elsa-make-type Int -> (List Int)) :to-equal
              [eieio-class-tag--elsa-function-type
               ([eieio-class-tag--elsa-type-int])
               [eieio-class-tag--elsa-type-list
                [eieio-class-tag--elsa-type-int]
                [eieio-class-tag--elsa-type-int]
                [eieio-class-tag--elsa-type-int]]]))

    (it "should"
      (expect (elsa-make-type Int -> Cons String Nil) :to-equal
              [eieio-class-tag--elsa-function-type
               ([eieio-class-tag--elsa-type-int])
               [eieio-class-tag--elsa-type-cons
                [eieio-class-tag--elsa-type-string]
                [eieio-class-tag--elsa-type-nil]]]))

    (it "should"
      (expect (elsa-make-type Int | Bool) :to-equal
              [eieio-class-tag--elsa-sum-type
               ([eieio-class-tag--elsa-type-bool]
                [eieio-class-tag--elsa-type-int])]))

    (it "should"
      (expect (elsa-make-type Int | Bool -> Float) :to-equal
              [eieio-class-tag--elsa-function-type
               ([eieio-class-tag--elsa-sum-type
                 ([eieio-class-tag--elsa-type-bool]
                  [eieio-class-tag--elsa-type-int])])
               [eieio-class-tag--elsa-type-float]]))

    (it "should"
      (expect (elsa-make-type Int | (Bool -> Float)) :to-equal
              [eieio-class-tag--elsa-sum-type
               ([eieio-class-tag--elsa-function-type
                 ([eieio-class-tag--elsa-type-bool])
                 [eieio-class-tag--elsa-type-float]]
                [eieio-class-tag--elsa-type-int])]))

    (it "should"
      (expect (elsa-make-type Int | (Bool -> Float | String)) :to-equal
              [eieio-class-tag--elsa-sum-type
               ([eieio-class-tag--elsa-function-type
                 ([eieio-class-tag--elsa-type-bool])
                 [eieio-class-tag--elsa-sum-type
                  ([eieio-class-tag--elsa-type-string]
                   [eieio-class-tag--elsa-type-float])]]
                [eieio-class-tag--elsa-type-int])]))

    (it "should"
      (expect (elsa-make-type Int -> Bool -> Cons (Int | String) Buffer) :to-equal
              [eieio-class-tag--elsa-function-type
               ([eieio-class-tag--elsa-type-int]
                [eieio-class-tag--elsa-type-bool])
               [eieio-class-tag--elsa-type-cons
                [eieio-class-tag--elsa-sum-type
                 ([eieio-class-tag--elsa-type-string]
                  [eieio-class-tag--elsa-type-int])]
                [eieio-class-tag--elsa-type-buffer]]]))

    (it "should"
      (expect (elsa-make-type Int -> (String -> Float) -> Buffer -> Cons (Int | String) Buffer) :to-equal
              [eieio-class-tag--elsa-function-type
               ([eieio-class-tag--elsa-type-int]
                [eieio-class-tag--elsa-function-type
                 ([eieio-class-tag--elsa-type-string])
                 [eieio-class-tag--elsa-type-float]]
                [eieio-class-tag--elsa-type-buffer])
               [eieio-class-tag--elsa-type-cons
                [eieio-class-tag--elsa-sum-type
                 ([eieio-class-tag--elsa-type-string]
                  [eieio-class-tag--elsa-type-int])]
                [eieio-class-tag--elsa-type-buffer]]])))

  (describe "elsa-type-sum"

    (it "should not create sum type if other regular type is already accepted by this"
      (let ((number (elsa-make-type Number))
            (int (elsa-make-type Int)))
        (expect (eieio-object-class (elsa-type-sum number int))
                :to-be 'elsa-type-number)))


    (it "should not create sum type if other sum type is already accepted by this"
      (let ((number (elsa-make-type Number))
            (int-or-float (elsa-make-type Int Float)))
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
        (expect (length (oref sum types)) :to-equal 2)))))
