;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-types)

(describe "Elsa Types"

  (describe "Numeric types"

    (describe "Acceptance rules"
      (it "number should accept int"
        (expect (elsa-type-accept (elsa-make-type number) (elsa-make-type int)) :to-be t))

      (it "number should accept float"
        (expect (elsa-type-accept (elsa-make-type number) (elsa-make-type float)) :to-be t))

      (it "float should accept int"
        (expect (elsa-type-accept (elsa-make-type float) (elsa-make-type int)) :to-be t)))

    (describe "Sum rules"
      (it "number + number should be number"
        (expect (elsa-type-sum (elsa-make-type number) (elsa-make-type number)) :to-be-type-equivalent
                (elsa-make-type number)))

      (it "number + float should be number"
        (expect (elsa-type-sum (elsa-make-type number) (elsa-make-type float)) :to-be-type-equivalent
                (elsa-make-type number)))

      (it "number + int should be number"
        (expect (elsa-type-sum (elsa-make-type number) (elsa-make-type int)) :to-be-type-equivalent
                (elsa-make-type number)))

      (it "float + number should be number"
        (expect (elsa-type-sum (elsa-make-type float) (elsa-make-type number)) :to-be-type-equivalent
                (elsa-make-type number)))

      (it "float + float should be float"
        (expect (elsa-type-sum (elsa-make-type float) (elsa-make-type float)) :to-be-type-equivalent
                (elsa-make-type float)))

      (it "float + int should be number"
        (expect (elsa-type-sum (elsa-make-type float) (elsa-make-type int)) :to-be-type-equivalent
                (elsa-make-type number)))

      (it "int + number should be number"
        (expect (elsa-type-sum (elsa-make-type int) (elsa-make-type number)) :to-be-type-equivalent
                (elsa-make-type number)))

      (it "int + float should be number"
        (expect (elsa-type-sum (elsa-make-type int) (elsa-make-type float)) :to-be-type-equivalent
                (elsa-make-type number)))

      (it "int + int should be int"
        (expect (elsa-type-sum (elsa-make-type int) (elsa-make-type int)) :to-be-type-equivalent
                (elsa-make-type int))))

    (describe "Intersection rules"
      (it "number & number should be number"
        (expect (elsa-type-intersect (elsa-make-type number) (elsa-make-type number)) :to-be-type-equivalent
                (elsa-make-type number)))

      (it "number & float should be float"
        (expect (elsa-type-intersect (elsa-make-type number) (elsa-make-type float)) :to-be-type-equivalent
                (elsa-make-type float)))

      (it "number & int should be int"
        (expect (elsa-type-intersect (elsa-make-type number) (elsa-make-type int)) :to-be-type-equivalent
                (elsa-make-type int)))

      (it "float & number should be float"
        (expect (elsa-type-intersect (elsa-make-type float) (elsa-make-type number)) :to-be-type-equivalent
                (elsa-make-type float)))

      (it "float & float should be float"
        (expect (elsa-type-intersect (elsa-make-type float) (elsa-make-type float)) :to-be-type-equivalent
                (elsa-make-type float)))

      (it "float & int should be empty"
        (expect (elsa-type-intersect (elsa-make-type float) (elsa-make-type int)) :to-be-type-equivalent
                (elsa-make-type empty)))

      (it "int & number should be int"
        (expect (elsa-type-intersect (elsa-make-type int) (elsa-make-type number)) :to-be-type-equivalent
                (elsa-make-type int)))

      (it "int & float should be empty"
        (expect (elsa-type-intersect (elsa-make-type int) (elsa-make-type float)) :to-be-type-equivalent
                (elsa-make-type empty)))

      (it "int & int should be int"
        (expect (elsa-type-intersect (elsa-make-type int) (elsa-make-type int)) :to-be-type-equivalent
                (elsa-make-type int))))

    (describe "Diff rules"
      (it "number - number should be empty"
        (expect (elsa-type-diff (elsa-make-type number) (elsa-make-type number)) :to-be-type-equivalent
                (elsa-make-type empty)))

      (it "number - float should be int"
        (expect (elsa-type-diff (elsa-make-type number) (elsa-make-type float)) :to-be-type-equivalent
                (elsa-make-type int)))

      (it "number - int should be float"
        (expect (elsa-type-diff (elsa-make-type number) (elsa-make-type int)) :to-be-type-equivalent
                (elsa-make-type float)))

      (it "float - number should be empty"
        (expect (elsa-type-diff (elsa-make-type float) (elsa-make-type number)) :to-be-type-equivalent
                (elsa-make-type empty)))

      (it "float - float should be empty"
        (expect (elsa-type-diff (elsa-make-type float) (elsa-make-type float)) :to-be-type-equivalent
                (elsa-make-type empty)))

      (it "float - int should be float"
        (expect (elsa-type-diff (elsa-make-type float) (elsa-make-type int)) :to-be-type-equivalent
                (elsa-make-type float)))

      (it "int - number should be empty"
        (expect (elsa-type-diff (elsa-make-type int) (elsa-make-type number)) :to-be-type-equivalent
                (elsa-make-type empty)))

      (it "int - float should be int"
        (expect (elsa-type-diff (elsa-make-type int) (elsa-make-type float)) :to-be-type-equivalent
                (elsa-make-type int)))

      (it "int - int should be empty"
        (expect (elsa-type-diff (elsa-make-type int) (elsa-make-type int)) :to-be-type-equivalent
                (elsa-make-type empty))))))
