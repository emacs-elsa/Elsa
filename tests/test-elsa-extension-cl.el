;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-reader)
(require 'elsa-types)
(require 'elsa-extension-cl)

(describe "Elsa extension"

  (describe "cl"

    (describe "defstruct"

      (describe "name resolution"

        (it "should parse name from a symbol"
          (elsa-test-with-analysed-form "|(cl-defstruct a name)" form
            :state-var state
            (expect (elsa-state-get-defstruct state 'a) :to-be-truthy)))

        (it "should parse name from a list"
          (elsa-test-with-analysed-form "|(cl-defstruct (a) name)" form
            :state-var state
            (expect (elsa-state-get-defstruct state 'a) :to-be-truthy))))

      (describe "constructor"

        (it "should register the default constructor"
          (elsa-test-with-analysed-form "|(cl-defstruct (a) name)" form
            :state-var state
            (expect (elsa-state-get-defun state 'make-a) :to-be-truthy)))

        (it "should not register the default constructor if it is disabled"
          (elsa-test-with-analysed-form "|(cl-defstruct (a (:constructor nil)) name)" form
            :state-var state
            (expect (elsa-state-get-defun state 'make-a) :not :to-be-truthy)))

        (it "should register alternative constructor"
          (elsa-test-with-analysed-form "|(cl-defstruct (a (:constructor create-a)) name)" form
            :state-var state
            (expect (elsa-state-get-defun state 'create-a) :to-be-truthy))))

      (it "should register a struct predicate"
        (elsa-test-with-analysed-form "|(cl-defstruct person name)" form
          :state-var state
          (expect (elsa-state-get-defun state 'person-p) :to-be-truthy)))

      (describe "slot types"

        (it "should resolve a simple type"
          (elsa-test-with-analysed-form "|(cl-defstruct (a) (slot nil :type string))" form
            :state-var state
            (expect (oref
                     (gethash
                      'slot
                      (oref (elsa-state-get-defstruct state 'a) slots))
                     type)
                    :to-be-type-equivalent (elsa-make-type string))))

        (it "should resolve a list-of type"
          (elsa-test-with-analysed-form "|(cl-defstruct (a) (slot nil :type (list-of string)))" form
            :state-var state
            (expect (oref
                     (gethash
                      'slot
                      (oref (elsa-state-get-defstruct state 'a) slots))
                     type)
                    :to-be-type-equivalent (elsa-make-type (list string)))))

        (it "should default to mixed"
          (elsa-test-with-analysed-form "|(cl-defstruct (a) (slot nil :type asdasdasd))" form
            :state-var state
            (expect (oref
                     (gethash
                      'slot
                      (oref (elsa-state-get-defstruct state 'a) slots))
                     type)
                    :to-be-type-equivalent (elsa-make-type mixed))))

        (it "should resolve a readonly type"
          (elsa-test-with-analysed-form "|(cl-defstruct (a) (slot nil :type string :read-only t))" form
            :state-var state
            (let ((type (oref
                         (gethash 'slot (oref (elsa-state-get-defstruct state 'a) slots))
                         type)))
              (expect type :to-be-type-equivalent (elsa-make-type (readonly string))))))

        (it "should warn about missing default type"
          (elsa-test-with-analysed-form "|(cl-defstruct (a) (slot :type asdasdasd))" form
            :errors-var errors
            (expect (car errors) :message-to-match "Second argument of a slot definition list should be the default value, not an option :type"))))

      (describe "slot accessors"

        (it "should register a slot accessor"
          (elsa-test-with-analysed-form "|(cl-defstruct (a) (slot nil :type string))" form
            :state-var state
            (expect (elsa-function-type-nth-arg (oref (elsa-state-get-defun state 'a-slot) type) 0)
                    :to-be-type-equivalent (elsa-make-type (struct a)))
            (expect (elsa-type-get-return (oref (elsa-state-get-defun state 'a-slot) type))
                    :to-be-type-equivalent (elsa-make-type string))))))

    (describe "defgeneric"

      (describe "method registration"

        (it "should register annotated empty generic to state and add arglist"
          (elsa-test-with-analysed-form "|;; (foo :: (function (string) string))\n(cl-defgeneric foo (a) \"docstring\")" form
            :state-var state
            (let ((def (elsa-state-get-defun state 'foo)))
              (expect (oref def arglist) :to-equal (list 'a))
              (expect (oref def type) :to-be-type-equivalent
                      (elsa-make-type (function (string) string))))))

        (it "should register unannotated empty generic to state and add arglist"
          (elsa-test-with-analysed-form "|(cl-defgeneric foo (a) \"docstring\")" form
            :state-var state
            (let ((def (elsa-state-get-defun state 'foo)))
              (expect (oref def arglist) :to-equal (list 'a))
              (expect (oref def type) :to-be-type-equivalent
                      (elsa-make-type (function (mixed) unbound))))))

        )
      )

    (describe "defmethod"

      (describe "resolving generic specifiers"

        (it "should resolve simple type"
          (elsa-test-with-analysed-form "|(cl-defmethod foo ((name symbol)) t)" form
            :state-var state
            (let ((name-form (elsa-car (elsa-car (elsa-nth 2 form)))))
              (expect (oref name-form type) :to-be-type-equivalent
                      (elsa-make-type symbol)))))

        (it "should resolve a class"
          (let ((state (elsa-state)))
            (elsa-state-add-defclass state
              (elsa-defclass
               :name 'elsa-form
               :slots (make-hash-table)
               :parents '((elsa-form))))
            (elsa-test-with-analysed-form "|(cl-defmethod foo ((name elsa-form)) t)" form
              :state state
              (let ((name-form (elsa-car (elsa-car (elsa-nth 2 form)))))
                (expect (oref name-form type) :to-be-type-equivalent
                        (elsa-make-type (class elsa-form))))))))

      (xdescribe "method registration"))))
