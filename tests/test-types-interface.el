;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-state)
(require 'elsa-types)

(describe "Elsa Types"

  (describe "elsa-interface"

    (describe "Declaration"

      (it "should be declared by defconst named elsa-interface-xyz"
        (unwind-protect
            (elsa-test-with-analysed-form "(defconst elsa-interface-xyz '(has-name bool))" form
              (expect (elsa-interface-p (get 'xyz 'elsa-interface))))
          (put 'xyz 'elsa-interface nil))))

    (xdescribe "Subtype rules")

    (xdescribe "Could accept rules")

    (describe "Methods"

      (describe "elsa-get-slots"
        (it "should get slot from this type"
          (unwind-protect
              (progn
                (put 'has-name 'elsa-interface (elsa-make-type (interface has-name name symbol)))
                (put 'foldable 'elsa-interface (elsa-make-type (interface foldable can-fold bool &extends has-name)))
                (let ((foldable (get 'foldable 'elsa-interface)))
                  (expect (oref (elsa-get-slot foldable 'can-fold) type)
                          :to-be-type-equivalent (elsa-type-bool))))
            (put 'has-name 'elsa-interface nil)
            (put 'foldable 'elsa-interface nil)))

        (it "should get slot from parent type"
          (unwind-protect
              (progn
                (put 'has-name 'elsa-interface (elsa-make-type (interface has-name name symbol)))
                (put 'foldable 'elsa-interface (elsa-make-type (interface foldable can-fold bool &extends has-name)))
                (let ((foldable (get 'foldable 'elsa-interface)))
                  (expect (oref (elsa-get-slot foldable 'name) type)
                          :to-be-type-equivalent (elsa-type-symbol))))
            (put 'has-name 'elsa-interface nil)
            (put 'foldable 'elsa-interface nil))))

      (xdescribe "elsa-type-get-args")

      (xdescribe "elsa-type-get-return")

      (xdescribe "elsa-type-composite-p")

      (xdescribe "elsa-type-callable-p")

      (xdescribe "elsa-function-type-nth-arg"))

    (xdescribe "Sum rules")

    (xdescribe "Intersection rules")

    (xdescribe "Diff rules")

    (xdescribe "Type normalization rules")))
