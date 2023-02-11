;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-types)

(describe "Elsa Variables"

  (describe "elsa-variables-group-and-sum"

    (it "should group variables by name and sum the types"
      (let* ((a1 (elsa-variable :name 'a :type (elsa-type-int)))
             (a2 (elsa-variable :name 'a :type (elsa-type-string)))
             (b1 (elsa-variable :name 'b :type (elsa-type-float)))
             (b2 (elsa-variable :name 'b :type (elsa-type-bool)))
             (grouped (elsa-variables-group-and-sum (list a1 a2 b1 b2))))
        (expect (length grouped) :to-be 2)
        (expect (oref (car grouped) name) :to-be 'a)
        (expect (elsa-type-equivalent-p
                 (oref (car grouped) type)
                 (elsa-make-type (or string int)))
                :to-be-truthy)
        (expect (elsa-type-equivalent-p
                 (oref (cadr grouped) type)
                 (elsa-make-type (or float bool)))
                :to-be-truthy)))))
