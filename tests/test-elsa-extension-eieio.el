;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-reader)
(require 'elsa-types)
(require 'elsa-extension-eieio)

(describe "Elsa extension"

  (describe "eieio"

    (describe "defclass slot types"

      (it "should resolve a simple type"
        (elsa-test-with-analysed-form "|(defclass a () ((slot :type string)))" form
          :state-var state
          (expect (oref
                   (gethash
                    'slot
                    (oref (elsa-state-get-defclass state 'a) slots))
                   type)
                  :to-be-type-equivalent (elsa-make-type string))))

      (it "should resolve a list-of type"
        (elsa-test-with-analysed-form "|(defclass a () ((slot :type (list-of string))))" form
          :state-var state
          (expect (oref
                   (gethash
                    'slot
                    (oref (elsa-state-get-defclass state 'a) slots))
                   type)
                  :to-be-type-equivalent (elsa-make-type (list string)))))

      (it "should default to mixed"
        (elsa-test-with-analysed-form "|(defclass a () ((slot :type asdasdasd)))" form
          :state-var state
          (expect (oref
                   (gethash
                    'slot
                    (oref (elsa-state-get-defclass state 'a) slots))
                   type)
                  :to-be-type-equivalent (elsa-make-type mixed)))))))
