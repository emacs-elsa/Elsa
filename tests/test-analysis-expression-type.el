;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa)

(xdescribe "Expression type resolution"


  (describe "Working with variables"

    (it "should recognize defvars"
      (let* ((state (elsa-state)))
        (elsa-state-add-defvar state 'a (elsa-make-type string))
        (expect (eieio-object-class
                 (elsa--get-expression-type state 'a))
                :to-equal 'elsa-type-string)))

    (it "should prefer lexically-bound variables over defvars"
      (let* ((state (elsa-state))
             (scope (oref state scope)))
        (elsa-scope-add-var
         scope (elsa-variable :name 'a :type (elsa-make-type string)))
        (elsa-state-add-defvar state 'a (elsa-make-type int))
        (expect (eieio-object-class
                 (elsa--get-expression-type state 'a))
                :to-equal 'elsa-type-string))))


  (describe "Working with function calls"

    (it "should recognize return type of defun"
      (let* ((state (elsa-state)))
        (elsa-state-add-defun
         state (elsa-defun
                :name 'a :args nil
                :return-type (elsa-make-type string)))
        (expect (eieio-object-class
                 (elsa--get-expression-type state '(a "foo" "bar")))
                :to-equal 'elsa-type-string)))))
