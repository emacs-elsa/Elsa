;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-type-helpers)

(defmacro elsa-test-describe-type (&rest spec)
  `(elsa-type-describe (elsa-make-type ,@spec)))

(describe "Elsa type helpers"

  (describe "elsa-make-type"

    (it "should make a primitive type"
      (expect (elsa-test-describe-type int) :to-equal "int"))

    (it "should make a simple function type"
      (expect (elsa-test-describe-type (function (int) int)) :to-equal "(function (int) int)"))

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
      (expect (elsa-test-describe-type (function (int bool) (cons (or int string) buffer))) :to-equal
              "(function (int bool) (cons (or int string) buffer))"))

    (it "should"
      (expect (elsa-test-describe-type (function (int (function (string) float) buffer) (cons (or int string) buffer))) :to-equal
              "(function (int (function (string) float) buffer) (cons (or int string) buffer))"))

    (it "should support construction of lists"
      (expect (elsa-test-describe-type (list int)) :to-equal "(list int)"))

    (it "should support construction of lists of lists"
      (expect (elsa-test-describe-type (list (list int))) :to-equal "(list (list int))"))

    (it "should support construction of lists of functions"
      (expect (elsa-test-describe-type (list (function (int) bool))) :to-equal "(list (function (int) bool))"))

    (it "should support construction of lists of sums"
      (expect (elsa-test-describe-type (list (or int bool))) :to-equal "(list (or int bool))"))

    (it "should support construction of lists of higher order types"
      (expect (elsa-test-describe-type (list (cons int string))) :to-equal
              "(list (cons int string))"))

    (it "should normalize the diff type after creation"
      (expect (elsa-type-describe (elsa-make-type (diff int (const "foo"))))
              :to-equal "int"))

    (describe "constant type creation"

      (it "should create a constant symbol from quoted symbol"
        (expect (elsa-type-describe (elsa-make-type 'foo))
                :to-equal "(const foo)"))

      (it "should throw an error for unquoted symbol which is not a type"
        (expect (elsa-type-describe (elsa-make-type foo))
                :to-throw 'error))

      (it "should create a constant symbol from a string"
        (expect (elsa-type-describe (elsa-make-type "foo"))
                :to-equal "(const \"foo\")"))

      (it "should create a constant symbol from quoted string"
        (expect (elsa-type-describe (elsa-make-type '"foo"))
                :to-equal "(const \"foo\")"))))

  (describe "elsa-type-normalize"

    (describe "diff type"

      (it "should normalize diff type to only positive if negative is not a subtype"
        (expect (elsa-type-describe
                 (elsa-type-normalize
                  (elsa-make-type (diff int (const :kek)))))
                :to-equal "int"))

      (it "should normalize diff type to empty if negative is super set of positive"
        (expect (elsa-type-normalize (elsa-make-type (diff (const 4) (or int (const 2)))))
                :to-be-type-equivalent (elsa-type-empty)))

      (it "should normalize neg to the intersection of pos and neg"
        (expect (elsa-make-type (diff string (or int (const "foo"))))
                :to-be-type-equivalent
                (elsa-make-type (diff string (const "foo")))))))

  (describe "elsa-type-equivalent-p"

    (it "empty sum and empty type are equivalent"
      (expect (elsa-type-equivalent-p (elsa-sum-type) (elsa-type-empty))
              :to-be-truthy)

      (expect (elsa-type-equivalent-p (elsa-type-empty) (elsa-sum-type))
              :to-be-truthy)))

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
