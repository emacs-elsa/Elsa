;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa)

(describe "Expression type resolution"


  (describe "Working with variables"


    (it "should recognize unbound variables"
      (expect (eieio-object-class
               (elsa--get-expression-type (elsa-state "") 'a))
              :to-equal 'elsa-type-unbound))

    (it "should recognize lexically-bound variables"
      (let* ((state (elsa-state ""))
             (scope (oref state scope)))
        (elsa-scope-add-variable
         scope (elsa-variable "" :name 'a :type (elsa-make-type 'string)))
        (expect (eieio-object-class
                 (elsa--get-expression-type state 'a))
                :to-equal 'elsa-type-string)))

    (it "should recognize defvars"
      (let* ((state (elsa-state "")))
        (elsa-state-add-defvar state 'a (elsa-make-type 'string))
        (expect (eieio-object-class
                 (elsa--get-expression-type state 'a))
                :to-equal 'elsa-type-string)))

    (it "should prefer lexically-bound variables over defvars"
      (let* ((state (elsa-state ""))
             (scope (oref state scope)))
        (elsa-scope-add-variable
         scope (elsa-variable "" :name 'a :type (elsa-make-type 'string)))
        (elsa-state-add-defvar state 'a (elsa-make-type 'int))
        (expect (eieio-object-class
                 (elsa--get-expression-type state 'a))
                :to-equal 'elsa-type-string)))

    (it "should not recognize nil as unbound variable"
      (expect (eieio-object-class
               (elsa--get-expression-type (elsa-state "") nil))
              :to-equal 'elsa-type-nil))

    (it "should not recognize t as unbound variable"
      (expect (elsa-type-accept
               (elsa--get-expression-type (elsa-state "") t)
               (elsa-make-type 'mixed))
              :to-be-truthy))

    (it "should not recognize keywords as unbound variable"
      (expect (elsa-type-accept
               (elsa--get-expression-type (elsa-state "") :foo)
               (elsa-make-type 'mixed))
              :to-be-truthy)))


  (describe "Working with function calls"


    (it "should recognize return type of defun"
      (let* ((state (elsa-state "")))
        (elsa-state-add-defun
         state (elsa-defun
                "" :name 'a :args nil
                :return-type (elsa-make-type 'string)))
        (expect (eieio-object-class
                 (elsa--get-expression-type state '(a "foo" "bar")))
                :to-equal 'elsa-type-string)))))
