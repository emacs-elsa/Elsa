;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa-types)

(describe "Elsa Types"


  (describe "Make type"

    (it "should make a simple type"
      (let ((type (elsa-make-type 'string)))
        (expect (elsa-type-string-p type) :to-be-truthy)
        (expect (elsa-type-nullable-p type) :not :to-be-truthy)))

    (it "should make a simple nullable type"
      (let ((type (elsa-make-type 'string?)))
        (expect (elsa-sum-type-p type) :to-be-truthy)
        (expect (elsa-type-accept type (elsa-make-type 'string)) :to-be-truthy)
        (expect (elsa-type-nullable-p type) :to-be-truthy)))

    (it "should not make unknown type"
      (expect (lambda ()
                (elsa-make-type 'ratherimporbablenameofatype))
              :to-throw))

    (it "should make a sum type from an &or definition"
      (let ((type (elsa-make-type [&or string int])))
        (expect (elsa-sum-type-p type) :to-be-truthy)
        (expect (elsa-type-accept type (elsa-make-type 'string)) :to-be-truthy)
        (expect (elsa-type-accept type (elsa-make-type 'int)) :to-be-truthy)
        (expect (elsa-type-accept type (elsa-make-type 'float)) :not :to-be-truthy))))


  (describe "Test type hierarchy"


    (it "should recognize subtypes reflexively"
      (expect (elsa-instance-of (elsa-make-type 'int) (elsa-make-type 'int))))

    (it "should recognize proper subtypes"
      (expect (elsa-instance-of (elsa-make-type 'int) 'number)))

    (it "should recognize abstract classes"
      (expect (elsa-instance-of (elsa-make-type 'int) 'elsa-type)))

    ;; TODO: add some testing trait type?
    ;; (it "should recognize traits"
    ;;   (expect (elsa-instance-of
    ;;            (elsa-make-type 'string)
    ;;            'elsa-type-trait-just-nullable)))


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
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'string)))
        (expect (length (oref sum types)) :to-equal 2)))

    (it "should accept a type if it is accepted by at least one type of the sum"
      (let ((sum (elsa-sum-type "")))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'string)))
        (expect (elsa-type-accept sum (elsa-make-type 'int)) :to-be-truthy)))

    (it "should accept nil type if nullable"
      (let ((sum (elsa-sum-type "")))
        ;; TODO: nil followed by a string can be normalized to
        ;; nullable string
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'nil)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'string)))
        (expect (elsa-type-accept sum (elsa-make-type 'nil)) :to-be-truthy)))

    (it "should accept nil type if it holds just nil"
      (let ((sum (elsa-sum-type "")))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'nil)))
        (expect (elsa-type-accept sum (elsa-make-type 'nil)) :to-be-truthy)))

    (it "should accept a type if it is accepted by at least one nullable type of the sum"
      (let ((sum (elsa-sum-type "")))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int?)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'string)))
        (expect (elsa-type-accept sum (elsa-make-type 'int)) :to-be-truthy)))

    (it "should not accept a nullable type if it is not accepted by the same nullable type int the sum"
      (let ((sum (elsa-sum-type "")))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'string)))
        (expect (elsa-type-accept sum (elsa-make-type 'int?)) :not :to-be-truthy)))

    (it "should not accept a type if it is not accepted by at least one type of the sum"
      (let ((sum (elsa-sum-type "")))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'string)))
        (expect (elsa-type-accept sum (elsa-make-type 'float)) :not :to-be-truthy)))

    (it "should be nullable if some type in the sum is nullable"
      (let ((sum (elsa-sum-type "")))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'string?)))
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should be nullable if nil type is added"
      (let ((sum (elsa-sum-type "")))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'nil)))
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should accept a sum type which is a subset of itself"
      (let ((sumA (elsa-make-type 'sum))
            (sumB (elsa-make-type 'sum)))
        (setq sumA (elsa-type-combine-with sumA (elsa-make-type 'string)))
        (setq sumA (elsa-type-combine-with sumA (elsa-make-type 'int)))
        (setq sumA (elsa-type-combine-with sumA (elsa-make-type 'float)))
        (setq sumB (elsa-type-combine-with sumB (elsa-make-type 'string)))
        (setq sumB (elsa-type-combine-with sumB (elsa-make-type 'int)))
        (expect (elsa-type-accept sumA sumB) :to-be-truthy)))

    (it "should not accept a sum type which is not a subset of itself"
      (let ((sumA (elsa-make-type 'sum))
            (sumB (elsa-make-type 'sum)))
        (setq sumA (elsa-type-combine-with sumA (elsa-make-type 'string)))
        (setq sumA (elsa-type-combine-with sumA (elsa-make-type 'int)))
        (setq sumB (elsa-type-combine-with sumB (elsa-make-type 'float)))
        (setq sumB (elsa-type-combine-with sumB (elsa-make-type 'string)))
        (setq sumB (elsa-type-combine-with sumB (elsa-make-type 'int)))
        (expect (elsa-type-accept sumA sumB) :not :to-be-truthy)))

    (it "should not add the same primitive type twice to the union"
      (let ((sum (elsa-make-type 'sum)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (expect (length (oref sum types)) :to-equal 1)))

    (it "should do nothing when adding a non-nullable type already present as nullable"
      (let ((sum (elsa-make-type 'sum)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int?)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (expect (length (oref sum types)) :to-equal 2)
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should make the nullable and then do nothing when adding a nullable type when non-nullable is already present"
      (let ((sum (elsa-make-type 'sum)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int?)))
        (expect (length (oref sum types)) :to-equal 2)
        (expect (elsa-type-nullable-p sum) :to-be-truthy))))


  (describe "Mixed type"


    (it "should accept any proper type"
      (expect (elsa-type-accept (elsa-make-type 'mixed)
                                (elsa-make-type 'string))
              :to-be-truthy))

    (it "should accept nullable types by default"
      (expect (elsa-type-accept (elsa-make-type 'mixed)
                                (elsa-make-type 'string?))
              :to-be-truthy))

    (it "should not accept nullable if it was made non-nullable"
      (let ((instance (elsa-make-type 'mixed)))
        (elsa-type-make-non-nullable instance)
        (expect (elsa-type-accept instance (elsa-make-type 'string?))
                :not :to-be-truthy)))

    (it "should accept non-nullable if it was made non-nullable"
      (let ((instance (elsa-make-type 'mixed)))
        (elsa-type-make-non-nullable instance)
        (expect (elsa-type-accept instance (elsa-make-type 'string))
                :to-be-truthy)))

    (it "should not accept unbound type"
      (expect (elsa-type-accept (elsa-make-type 'mixed)
                                (elsa-make-type 'unbound))
              :not :to-be-truthy))

    (it "is always created as nullable"
      (expect (elsa-type-nullable-p (elsa-make-type 'mixed))
              :to-be-truthy))

    (it "can be made non-nil"
      (let ((instance (elsa-make-type 'mixed)))
        (elsa-type-make-non-nullable instance)
        (expect (elsa-type-nullable-p instance) :not :to-be-truthy))))


  (describe "Simple type"


    (it "should not be possible to make non-nullable"
      (expect (lambda ()
                (elsa-type-make-non-nullable (elsa-make-type 'string)))
              :to-throw)))


  (describe "Just-nullable type"


    (it "should accept nil if nullable"
      (expect (elsa-type-accept (elsa-make-type 'string?)
                                (elsa-make-type 'nil))
              :to-be-truthy))

    (it "should not accept nil if not nullable"
      (expect (elsa-type-accept (elsa-make-type 'string)
                                (elsa-make-type 'nil))
              :not :to-be-truthy))

    (it "should accept non-nullable types if it is nullable"
      (expect (elsa-type-accept (elsa-make-type 'string?)
                                (elsa-make-type 'string))
              :to-be-truthy))

    (it "should not accept nullable types if it is non-nullable"
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
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'float)))
        (expect (elsa-type-accept (elsa-make-type 'number) sum) :to-be-truthy)))

    (it "should not accept sum type if it can not accept some type of the sum"
      (let ((sum (elsa-sum-type "")))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'int)))
        (setq sum (elsa-type-combine-with sum (elsa-make-type 'string)))
        (expect
         (elsa-type-accept (elsa-make-type 'number) sum) :not :to-be-truthy)))))
