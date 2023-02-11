;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-reader)
(require 'elsa-types)
(require 'elsa-extension-builtin)

(describe "Elsa extension"

  (describe "Builtin"

    (describe "function not"

      (it "should resolve to type t if input argument is definitely nil"
        (elsa-test-with-analysed-form "|(not nil)" form
          (expect (elsa-type-t-p (oref form type)) :to-be-truthy)))

      (it "should resolve to type nil if input argument is definitely t"
        (elsa-test-with-analysed-form "|(not t)" form
          (expect (elsa-type-nil-p (oref form type)) :to-be-truthy)))

      (it "should resolve to type bool if input argument is mixed"
        (elsa-test-with-analysed-form "|(not x)" form
          (expect (elsa-type-accept (elsa-make-type bool)
                                    (oref form type))
                  :to-be-truthy))))))
