;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-types)

(describe "Elsa Types"


  (describe "Make type"

    (it "should make a simple type"
      (let ((type (elsa-make-type string)))
        (expect (elsa-type-string-p type) :to-be-truthy)
        (expect (elsa-type-nullable-p type) :not :to-be-truthy)))

    (it "should make a simple nullable type"
      (let ((type (elsa-make-type (or string nil))))
        (expect (elsa-sum-type-p type) :to-be-truthy)
        (expect (elsa-type-accept type (elsa-make-type string)) :to-be-truthy)
        (expect (elsa-type-nullable-p type) :to-be-truthy)))

    (it "should not make unknown type"
      (expect (elsa-make-type ratherimporbablenameofatype) :to-throw))

    (it "should make a sum type from a sum definition"
      (let ((type (elsa-make-type (or string int))))
        (expect (elsa-sum-type-p type) :to-be-truthy)
        (expect (elsa-type-accept type (elsa-make-type string)) :to-be-truthy)
        (expect (elsa-type-accept type (elsa-make-type int)) :to-be-truthy)
        (expect (elsa-type-accept type (elsa-make-type float)) :not :to-be-truthy))))


  (describe "Test type hierarchy"


    (it "should recognize subtypes reflexively"
      (expect (elsa-instance-of (elsa-make-type int) (elsa-make-type int))))

    (it "should recognize proper subtypes"
      (expect (elsa-instance-of (elsa-make-type int) 'number)))


    (describe "Work with instances or types"


      (it "should compare two types directly"
        (expect (elsa-instance-of 'int 'number)))

      (it "should compare an instance and a type"
        (expect (elsa-instance-of (elsa-make-type int) 'number)))

      (it "should compare two instances"
        (expect (elsa-instance-of (elsa-make-type int) (elsa-make-type int))))))

  (describe "primitive type"

    (it "should accept itself without a constant"
      (expect (elsa-type-accept
               (elsa-make-type int)
               (elsa-make-type (diff int (const 4)))) :to-be-truthy))

    (it "without a constant should not accept itself"
      (expect (elsa-type-accept
               (elsa-make-type (diff int (const 4)))
               (elsa-make-type int)) :not :to-be-truthy))

    (it "float should accept int"
      (expect (elsa-type-accept
               (elsa-make-type float)
               (elsa-make-type int)) :to-be-truthy)))

  (describe "Cons type"

    (it "of mixed should accept any cons"
      (expect (elsa-type-accept
               (elsa-make-type (cons mixed mixed))
               (elsa-make-type (cons int string)))
              :to-be-truthy))

    (it "should be covariant in first type argument"
      (expect (elsa-type-accept
               (elsa-make-type (cons number string))
               (elsa-make-type (cons int string)))
              :to-be-truthy))

    (it "should be covariant in second type argument"
      (expect (elsa-type-accept
               (elsa-make-type (cons string number))
               (elsa-make-type (cons string int)))
              :to-be-truthy))

    (it "of should not accept if any of types is not a subtype of the other"
      (expect (elsa-type-accept
               (elsa-make-type (cons number string))
               (elsa-make-type (cons string int)))
              :not :to-be-truthy))

    (it "of should not accept if any of types is not a subtype of the other"
      (expect (elsa-type-accept
               (elsa-make-type (cons string number))
               (elsa-make-type (cons int string)))
              :not :to-be-truthy)))

  (describe "Empty type"

    (it "should accept empty type"
      (expect (elsa-type-accept (elsa-type-empty) (elsa-type-empty)) :to-be-truthy))

    (it "should accept empty sum type"
      (expect (elsa-type-accept (elsa-type-empty) (elsa-sum-type)) :to-be-truthy))

    (it "should not accept non-empty type"
      (expect (elsa-type-accept (elsa-type-empty) (elsa-type-int))
              :not :to-be-truthy))

    (it "should be accepted by non-empty type"
      (expect (elsa-type-accept (elsa-type-int) (elsa-type-empty)) :to-be-truthy)))

  (describe "Sum type"

    (describe "empty sum type"

      (it "should accept empty sum type"
        (expect (elsa-type-accept (elsa-sum-type) (elsa-sum-type)) :to-be-truthy))

      (it "should accept empty type"
        (expect (elsa-type-accept (elsa-sum-type) (elsa-type-empty)) :to-be-truthy))

      (it "should not accept non-empty type"
        (expect (elsa-type-accept (elsa-sum-type) (elsa-make-type int))
                :not :to-be-truthy))

      (it "should be accepted by non-empty type"
        (expect (elsa-type-accept (elsa-make-type int) (elsa-sum-type))
                :to-be-truthy)))

    (it "should accept diff type where positive is one of the summands"
      (expect (elsa-make-type (or int string)) :to-accept-type
              (elsa-make-type (diff int (const 4)))))

    (it "sum of constants should accept one of the constants"
      (expect (elsa-type-accept
               (elsa-make-type (or (const 1) (const 2)))
               (elsa-make-type (const 1))) :to-be-truthy))

    (it "sum of constant and primitive should accept the constant"
      (expect (elsa-type-accept
               (elsa-make-type (or (const 1) string))
               (elsa-make-type (const 1))) :to-be-truthy))

    (it "should not share data with its clone"
      (let* ((old (elsa-make-type (or int string)))
             (new (clone old)))
        (expect (eq old new) :not :to-be-truthy)
        (expect (eq (car (oref old types))
                    (car (oref new types))) :not :to-be-truthy)
        (oset new types nil)
        (expect (length (oref old types)) :to-be 2)
        (expect (length (oref new types)) :to-be 0)))

    (it "should be able to hold no types"
      (let ((sum (elsa-sum-type)))
        (expect (length (oref sum types)) :to-equal 0)))

    (it "should be able to hold more types"
      (let ((sum (elsa-type-sum (elsa-make-type int) (elsa-make-type string))))
        (expect (length (oref sum types)) :to-equal 2)))

    (it "should accept a type if it is accepted by at least one type of the sum"
      (let ((sum (elsa-type-sum (elsa-make-type int) (elsa-make-type string))))
        (expect (elsa-type-accept sum (elsa-make-type int)) :to-be-truthy)))

    (it "should accept nil type if nullable"
      (let ((sum (elsa-type-sum (elsa-make-type nil) (elsa-make-type string))))
        (expect (elsa-type-accept sum (elsa-make-type nil)) :to-be-truthy)))

    (it "should accept nil type if it holds just nil"
      (let ((sum (elsa-sum-type)))
        (setq sum (elsa-type-sum sum (elsa-make-type nil)))
        (expect (elsa-type-accept sum (elsa-make-type nil)) :to-be-truthy)))

    (it "should accept a type if it is accepted by at least one nullable type of the sum"
      (let ((sum (elsa-sum-type)))
        (setq sum (elsa-type-sum sum (elsa-make-type (or int nil))))
        (setq sum (elsa-type-sum sum (elsa-make-type string)))
        (expect (elsa-type-accept sum (elsa-make-type int)) :to-be-truthy)))

    (it "should not accept a nullable type if it is not accepted by the same nullable type int the sum"
      (let ((sum (elsa-sum-type)))
        (setq sum (elsa-type-sum sum (elsa-make-type int)))
        (setq sum (elsa-type-sum sum (elsa-make-type string)))
        (expect (elsa-type-accept sum (elsa-make-type (or int nil))) :not :to-be-truthy)))

    (it "should not accept a type if it is not accepted by at least one type of the sum"
      (let ((sum (elsa-sum-type)))
        (setq sum (elsa-type-sum sum (elsa-make-type int)))
        (setq sum (elsa-type-sum sum (elsa-make-type string)))
        (expect (elsa-type-accept sum (elsa-make-type float)) :not :to-be-truthy)))

    (it "should be nullable if some type in the sum is nullable"
      (let ((sum (elsa-sum-type)))
        (setq sum (elsa-type-sum sum (elsa-make-type int)))
        (setq sum (elsa-type-sum sum (elsa-make-type (or string nil))))
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should be nullable if nil type is added"
      (let ((sum (elsa-sum-type)))
        (setq sum (elsa-type-sum sum (elsa-make-type int)))
        (setq sum (elsa-type-sum sum (elsa-make-type nil)))
        (expect (elsa-type-nullable-p sum) :to-be-truthy)))

    (it "should accept a sum type which is a subset of itself"
      (let ((sumA (elsa-sum-type))
            (sumB (elsa-sum-type)))
        (setq sumA (elsa-type-sum sumA (elsa-make-type string)))
        (setq sumA (elsa-type-sum sumA (elsa-make-type int)))
        (setq sumA (elsa-type-sum sumA (elsa-make-type float)))
        (setq sumB (elsa-type-sum sumB (elsa-make-type string)))
        (setq sumB (elsa-type-sum sumB (elsa-make-type int)))
        (expect (elsa-type-accept sumA sumB) :to-be-truthy)))

    (it "should not accept a sum type which is not a subset of itself"
      (let ((sumA (elsa-sum-type))
            (sumB (elsa-sum-type)))
        (setq sumA (elsa-type-sum sumA (elsa-make-type string)))
        (setq sumA (elsa-type-sum sumA (elsa-make-type int)))
        (setq sumB (elsa-type-sum sumB (elsa-make-type float)))
        (setq sumB (elsa-type-sum sumB (elsa-make-type string)))
        (setq sumB (elsa-type-sum sumB (elsa-make-type int)))
        (expect (elsa-type-accept sumA sumB) :not :to-be-truthy)))

    (it "should accept intersection type containing one of the summands"
      (expect
       (elsa-sum-type :types (list (elsa-make-type int)
                                   (elsa-make-type symbol)))
       :to-accept-type
       (elsa-intersection-type :types (list (elsa-make-type int)
                                            (elsa-make-type string))))))

  (describe "Diff type"

    (it "should not accept types where the positive does not accept the other type"
      (expect (elsa-diff-type :positive (elsa-type-int))
              :not :to-accept-type (elsa-type-keyword)))

    (it "should not accept types where the negative is accepted by the other type"
      (expect (elsa-diff-type :negative (elsa-type-int))
              :not :to-accept-type (elsa-type-number)))

    (it "should accept itself"
      (expect (elsa-make-type (diff int (const 1))) :to-accept-type
              (elsa-make-type (diff int (const 1)))))

    (it "should not accept diff type where the other negative have intersection with this"
      (expect (elsa-make-type (diff int (const 2))) :not :to-accept-type
              (elsa-make-type (diff int (const 4)))))

    (it "should accept a constant which is not excluded"
      (expect (elsa-make-type (diff int (const 1))) :to-accept-type
              (elsa-make-type (const 2)))))

  (describe "Intersection type"

    (it "intersection should accept more specific intersection"
      (expect
       (elsa-intersection-type :types (list (elsa-make-type int)
                                            (elsa-make-type string)))
       :to-accept-type
       (elsa-intersection-type :types (list (elsa-make-type int)
                                            (elsa-make-type string)
                                            (elsa-make-type symbol)))))

    (it "should accept itself"
      (expect
       (elsa-intersection-type :types (list (elsa-make-type int)
                                            (elsa-make-type string)))
       :to-accept-type
       (elsa-intersection-type :types (list (elsa-make-type int)
                                            (elsa-make-type string))))))

  (describe "Mixed type"

    (it "should accept any proper type"
      (expect (elsa-type-accept (elsa-make-type mixed)
                                (elsa-make-type string))
              :to-be-truthy))

    (it "should accept nullable types by default"
      (expect (elsa-type-accept (elsa-make-type mixed)
                                (elsa-make-type (or string nil)))
              :to-be-truthy))

    (xit "should not accept nullable if it was made non-nullable"
      (let ((instance (elsa-make-type mixed)))
        (elsa-type-make-non-nullable instance)
        (expect (elsa-type-accept instance (elsa-make-type (or string nil)))
                :not :to-be-truthy)))

    (xit "should accept non-nullable if it was made non-nullable"
      (let ((instance (elsa-make-type mixed)))
        (elsa-type-make-non-nullable instance)
        (expect (elsa-type-accept instance (elsa-make-type string))
                :to-be-truthy)))

    (it "should not accept unbound type"
      (expect (elsa-type-accept (elsa-make-type mixed)
                                (elsa-make-type unbound))
              :not :to-be-truthy))

    (it "is always created as nullable"
      (expect (elsa-type-nullable-p (elsa-make-type mixed))
              :to-be-truthy))

    (xit "can be made non-nil"
      (let ((instance (elsa-make-type mixed)))
        (elsa-type-make-non-nullable instance)
        (expect (elsa-type-nullable-p instance) :not :to-be-truthy))))


  (describe "Simple type"

    (it "should not be changed by being made non-nullable"
      (expect (elsa-type-make-non-nullable (elsa-make-type string)) :to-equal (elsa-make-type string)))

    (it "should not share data with its clone"
      (let* ((old (elsa-make-type int))
             (new (clone old)))
        (expect (eq old new) :not :to-be-truthy))))

  (describe "Bool type"

    (it "should accept t"
      (expect (elsa-type-accept (elsa-type-bool) (elsa-type-t)) :to-be-truthy))

    (it "should accept nil"
      (expect (elsa-type-accept (elsa-type-bool) (elsa-type-t)) :to-be-truthy))

    (it "should accept t?"
      (expect (elsa-type-accept (elsa-type-bool) (elsa-make-type bool)) :to-be-truthy))

    (it "should accept bool"
      (expect (elsa-type-accept (elsa-type-bool) (elsa-type-bool)) :to-be-truthy))

    (it "should not accept int|bool"
      (expect (elsa-type-accept (elsa-type-bool) (elsa-make-type (or int bool))) :not :to-be-truthy))

    (it "should not accept int|t"
      (expect (elsa-type-accept (elsa-type-bool) (elsa-make-type (or int t))) :not :to-be-truthy)))

  (describe "Just-nullable type"

    (it "should accept nil if nullable"
      (expect (elsa-type-accept (elsa-make-type (or string nil))
                                (elsa-make-type nil))
              :to-be-truthy))

    (it "should not accept nil if not nullable"
      (expect (elsa-type-accept (elsa-make-type string)
                                (elsa-make-type nil))
              :not :to-be-truthy))

    (it "should accept non-nullable types if it is nullable"
      (expect (elsa-type-accept (elsa-make-type (or string nil))
                                (elsa-make-type string))
              :to-be-truthy))

    (it "should not accept nullable types if it is non-nullable"
      (expect (elsa-type-accept (elsa-make-type string)
                                (elsa-make-type (or string nil)))
              :not :to-be-truthy))

    (it "should accept subtypes"
      (expect (elsa-type-accept (elsa-make-type number)
                                (elsa-make-type int))
              :to-be-truthy))

    (it "should not accept supertypes"
      (expect (elsa-type-accept (elsa-make-type int)
                                (elsa-make-type number))
              :not :to-be-truthy))

    (it "should accept sum type if it accepts every type of the sum"
      (let ((sum (elsa-sum-type)))
        (setq sum (elsa-type-sum sum (elsa-make-type int)))
        (setq sum (elsa-type-sum sum (elsa-make-type float)))
        (expect (elsa-type-accept (elsa-make-type number) sum) :to-be-truthy)))

    (it "should not accept sum type if it can not accept some type of the sum"
      (let ((sum (elsa-sum-type)))
        (setq sum (elsa-type-sum sum (elsa-make-type int)))
        (setq sum (elsa-type-sum sum (elsa-make-type string)))
        (expect
         (elsa-type-accept (elsa-make-type number) sum) :not :to-be-truthy))))

  (describe "tuple type"

    (it "should not accept tuple of different length"
      (expect (elsa-type-accept (elsa-make-type (int int))
                                (elsa-make-type (int)))
              :not :to-be-truthy))

    (it "should accept tuple of same length and types"
      (expect (elsa-type-accept (elsa-make-type (int string))
                                (elsa-make-type (int string)))
              :to-be-truthy))

    (it "should not accept tuple of same length and different types"
      (expect (elsa-type-accept (elsa-make-type (int string))
                                (elsa-make-type (string int)))
              :not :to-be-truthy))

    (it "should accept tuple of same length and subtypes"
      (expect (elsa-type-accept (elsa-make-type ((or int string) string))
                                (elsa-make-type (string string)))
              :to-be-truthy))

    (it "should not accept tuple of same length and supertypes"
      (expect (elsa-type-accept (elsa-make-type (string string))
                                (elsa-make-type ((or int string) string)))
              :not :to-be-truthy))

    (it "should be accepted by a list of a supertype of all types of tuple"
      (expect (elsa-type-accept (elsa-make-type (list int))
                                (elsa-make-type (int int int)))
              :to-be-truthy))

    (it "should not be accepted by a list if some type is not acceptable by item-type of list"
      (expect (elsa-type-accept (elsa-make-type (list int))
                                (elsa-make-type (int string int)))
              :not :to-be-truthy))))
