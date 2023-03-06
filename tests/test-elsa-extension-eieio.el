;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-reader)
(require 'elsa-types)
(require 'elsa-extension-eieio)

(describe "Elsa extension"

  (describe "eieio"

    (xdescribe "checks"

      (xdescribe "check xyz"))

    (describe "analysis"

      (describe "defclass"

        (describe "constructor"

          (it "should declare a constructor"
            (elsa-test-with-analysed-form "|(defclass a () ((slot :type string)))" form
              :state-var state
              (expect (elsa-state-get-defun state 'a) :not :to-be nil))))

        (describe "type predicate"

          (it "should declare a type predicate"
            (elsa-test-with-analysed-form "|(defclass a () ((slot :type string)))" form
              :state-var state
              (expect (elsa-state-get-defun state 'a-p) :not :to-be nil))))

        (describe "accessor"

          (it "should register accessor as a new defun if it doesn't exist"
            (elsa-test-with-analysed-form "|(defclass a () ((slot :type string :accessor get-slot)))" form
              :state-var state
              (expect (elsa-state-get-defun state 'get-slot) :not :to-be nil)
              (expect (elsa-function-type-p (oref (elsa-state-get-defun state 'get-slot) type)))
              (expect (elsa-type-get-return (oref (elsa-state-get-defun state 'get-slot) type))
                      :to-be-type-equivalent (elsa-make-type string))))

          (it "should register accessor as a new overload if it already exists"
            (elsa-test-with-analysed-form "|(progn (defclass a () ((slot :type string :accessor get-slot))) (defclass b () ((slot :type number :accessor get-slot))))" form
              :state-var state
              (expect (elsa-state-get-defun state 'get-slot) :not :to-be nil)
              (expect (elsa-intersection-type-p (oref (elsa-state-get-defun state 'get-slot) type)))
              (expect (elsa-type-get-return (oref (elsa-state-get-defun state 'get-slot) type))
                      :to-be-type-equivalent (elsa-make-type (or string number))))))

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
                      :to-be-type-equivalent (elsa-make-type mixed))))))

      (describe "oref"

        (it "should signal error if the instance is not of class type"
          (elsa-test-with-analysed-form "|(let ((x \"foo\")) (oref x slot))" form
            :errors-var errors
            (expect (car errors) :message-to-match "Type `(const \"foo\")' has no properties because it is not a class.")))

        (it "should signal error if the class has no such slot"
          (elsa-test-with-analysed-form "|(progn (defclass a () ((name :type string))) (oref (a) slot))" form
            :errors-var errors
            (expect (car errors) :message-to-match "Type `(class a)' has no slot `slot', has (name)"))))

      (describe "oset"

        (it "should signal error if the instance is not of class type"
          (elsa-test-with-analysed-form "|(let ((x \"foo\")) (oset x slot 2))" form
            :errors-var errors
            (expect (car errors) :message-to-match "Type `(const \"foo\")' has no properties because it is not a class.")))

        (it "should signal error if the class has no such slot"
          (elsa-test-with-analysed-form "|(progn (defclass a () ((name :type string))) (oset (a) slot 2))" form
            :errors-var errors
            (expect (car errors) :message-to-match "Type `(class a)' has no slot `slot', has (name)")))

        (it "should signal error if the slot type does not accept value"
          (elsa-test-with-analysed-form "|(progn (defclass a () ((name :type string))) (oset (a) name 2))" form
            :errors-var errors
            (expect (car errors) :message-to-match "Property `name' can not accept type `(const 2)', has type `string'")))))))
