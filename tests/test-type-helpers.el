;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa-type-helpers)

(describe "Elsa type helpers"

  (describe "elsa-type-sum"

    (it "should not create sum type if other regular type is already accepted by this"
      (let ((number (elsa-make-type 'number))
            (int (elsa-make-type 'int)))
        (expect (eieio-object-class (elsa-type-sum number int))
                :to-be 'elsa-type-number)))


    (it "should not create sum type if other sum type is already accepted by this"
      (let ((number (elsa-make-type 'number))
            (int-or-float (elsa-make-type 'int 'float)))
        (expect (eieio-object-class (elsa-type-sum number int-or-float))
                :to-be 'elsa-type-number)))

    (it "should do nothing when adding a non-nullable type already present as nullable"
      (let ((sum (elsa-type-sum (elsa-make-type 'int?) (elsa-make-type 'int))))
        (expect (length (oref sum types)) :to-equal 2)
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should be made nullable and then do nothing when adding a nullable type when non-nullable is already present"
      (let ((sum (elsa-type-sum (elsa-make-type 'int) (elsa-make-type 'int?))))
        (expect (length (oref sum types)) :to-equal 2)
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should return original type if summed with itself"
      (let ((sum (elsa-type-sum (elsa-make-type 'int) (elsa-make-type 'int))))
        (expect (elsa-type-int-p sum) :to-be-truthy)))

    (it "should not add the same primitive type twice"
      (let ((sum (elsa-type-sum (elsa-make-type 'float) (elsa-make-type 'int))))
        (setq sum (elsa-type-sum sum (elsa-make-type 'int)))
        (expect (length (oref sum types)) :to-equal 2)))))
