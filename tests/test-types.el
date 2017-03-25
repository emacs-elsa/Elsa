;; -*- lexical-binding: t -*-

(require 'elsa-types)

(describe "Elsa Types"


  (describe "Make type"

    (it "should make a simple type"
      (let ((type (elsa-make-type 'string)))
        (expect (elsa-type-string-p type) :to-be-truthy)
        (expect (elsa-type-nullable-p type) :not :to-be-truthy)))

    (it "should make a simple nullable type"
      (let ((type (elsa-make-type 'string?)))
        (expect (elsa-type-string-p type) :to-be-truthy)
        (expect (elsa-type-nullable-p type) :to-be-truthy)))

    (it "should not make unknown type"
      (expect (lambda ()
                (elsa-make-type 'ratherimporbablenameofatype))
              :to-throw)))


  (describe "Test type hierarchy"


    (it "should recognize subtypes reflexively"
      (expect (elsa-instance-of (elsa-make-type 'int) (elsa-make-type 'int))))

    (it "should recognize proper subtypes"
      (expect (elsa-instance-of (elsa-make-type 'int) 'number)))

    (it "should recognize abstract classes"
      (expect (elsa-instance-of (elsa-make-type 'int) 'elsa-type)))

    (it "should recognize traits"
      (expect (elsa-instance-of
               (elsa-make-type 'string)
               'elsa-type-trait-just-nullable)))


    (describe "Work with instances or types"


      (it "should compare two types directly"
        (expect (elsa-instance-of 'int 'number)))

      (it "should compare an instance and a type"
        (expect (elsa-instance-of (elsa-make-type 'int) 'number)))

      (it "should compare two instances"
        (expect (elsa-instance-of (elsa-make-type 'int) (elsa-make-type 'int))))))


  (describe "Sum type"


    (it "should be able to hold no types"
      (let ((sum (elsa-sum-type "")))
        (expect (length (oref sum types)) :to-equal 0)))

    (it "should be able to hold more types"
      (let ((sum (elsa-sum-type "")))
        (elsa-sum-type-add sum (elsa-make-type 'int))
        (elsa-sum-type-add sum (elsa-make-type 'string))
        (expect (length (oref sum types)) :to-equal 2)))

    (it "should be nullable if some type in the sum is nullable"
      (let ((sum (elsa-sum-type "")))
        (elsa-sum-type-add sum (elsa-make-type 'int))
        (elsa-sum-type-add sum (elsa-make-type 'string?))
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should set itself nullable if nil type is added"
      (let ((sum (elsa-sum-type "")))
        (elsa-sum-type-add sum (elsa-make-type 'int))
        (elsa-sum-type-add sum (elsa-make-type 'nil))
        (expect (elsa-type-nullable-p sum) :to-be-truthy)
        (expect (length (oref sum types)) :to-equal 1))))


  (describe "Just-nullable type"


    (it "should accept nil if nullable"
      (expect (elsa-type-accept (elsa-make-type 'string?)
                                (elsa-make-type 'nil))
              :to-be-truthy))

    (it "should not accept nil if not nullable"
      (expect (elsa-type-accept (elsa-make-type 'string)
                                (elsa-make-type 'nil))
              :not :to-be-truthy))

    (it "should accept non-nullable types if they are nullable"
      (expect (elsa-type-accept (elsa-make-type 'string?)
                                (elsa-make-type 'string))
              :to-be-truthy))

    (it "should not accept nullable types if they are non-nullable"
      (expect (elsa-type-accept (elsa-make-type 'string)
                                (elsa-make-type 'string?))
              :not :to-be-truthy))

    (it "should accept subtypes"
      (expect (elsa-type-accept (elsa-make-type 'number)
                                (elsa-make-type 'int))
              :to-be-truthy))

    (it "should not accept supertypes"
      (expect (elsa-type-accept (elsa-make-type 'int)
                                (elsa-make-type 'number))
              :not :to-be-truthy))

    (it "should accept sum type if it accepts every type of the sum"
      (let ((sum (elsa-sum-type "")))
        (elsa-sum-type-add sum (elsa-make-type 'int))
        (elsa-sum-type-add sum (elsa-make-type 'float))
        (expect (elsa-type-accept (elsa-make-type 'number) sum) :to-be-truthy)))

    (it "should not accept sum type if it can not accept some type of the sum"
      (let ((sum (elsa-sum-type "")))
        (elsa-sum-type-add sum (elsa-make-type 'int))
        (elsa-sum-type-add sum (elsa-make-type 'string))
        (expect
         (elsa-type-accept (elsa-make-type 'number) sum) :not :to-be-truthy)))))
