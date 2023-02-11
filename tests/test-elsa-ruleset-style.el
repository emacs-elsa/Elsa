;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-reader)
(require 'elsa-types)
(require 'elsa-extension-builtin)
(require 'elsa-ruleset)

(describe "Ruleset"

  (describe "Style"

    (before-all
      (setq elsa-checks nil)
      (elsa-ruleset-load (elsa-ruleset-style)))

    (describe "Unnecessary progn"

      (it "should warn about useless progn around else branch"
        (elsa-test-with-analysed-form "|(if out 1 (progn 2))" form
          :errors-var errors
          (expect errors :not :to-be nil)
          (expect (oref (car errors) message) :to-equal
                  "Useless `progn' around body of else branch."))))

    (describe "Eta conversion"

      (it "should suggest converting a unary function"
        (elsa-test-with-analysed-form "|(lambda (x) (stringp x))" form
          :state-var state
          (expect (oref state errors) :not :to-be nil)
          (expect (oref (car (oref state errors)) message) :to-equal
                  "You can eta convert the lambda form and use the function `stringp' directly")))

      (it "should not suggest converting a unary function if argument names don't match"
        (elsa-test-with-analysed-form "|(let (y) (lambda (x) (stringp y)))" form
          :state-var state
          (expect (oref state errors) :to-be nil)))

      (it "should suggest converting a binary function"
        (elsa-test-with-analysed-form "|(lambda (x y) (equal x y))" form
          :state-var state
          (expect (oref state errors) :not :to-be nil)
          (expect (oref (car (oref state errors)) message) :to-equal
                  "You can eta convert the lambda form and use the function `equal' directly")))

      (it "should not suggest converting a binary function if arguments don't match"
        (elsa-test-with-analysed-form "|(lambda (x y) (equal y x))" form
          :state-var state
          (expect (oref state errors) :to-be nil))))))
