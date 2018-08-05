;; -*- lexical-binding: t -*-

;; (require 'elsa-check-if)
(require 'elsa-reader)

(require 'elsa-test-helpers)

(xdescribe "Elsa check if"

  (describe "Unreachable branch"

    (it "then should not be reachable if the condition is nil"
      (elsa-test-with-buffer "|(if nil 1 2)"
        (elsa-infer-types (elsa-read-form) (elsa-scope) (elsa-scope))
        (expect (length elsa-infer-errors) :to-be 1)
        (expect (oref (car elsa-infer-errors) message) :to-equal
                "Then clause will never be executed.")))

    (it "else should not be reachable if the condition is t"
      (elsa-test-with-buffer "|(if t 1 2)"
        (elsa-infer-types (elsa-read-form) (elsa-scope) (elsa-scope))
        (expect (length elsa-infer-errors) :to-be 1)
        (expect (oref (car elsa-infer-errors) message) :to-equal
                "Else clause will never be executed.")))

    (it "else should not be reachable if the condition is atom"
      (elsa-test-with-buffer "|(if \"string\" 1 2)"
        (elsa-infer-types (elsa-read-form) (elsa-scope) (elsa-scope))
        (expect (length elsa-infer-errors) :to-be 1)
        (expect (oref (car elsa-infer-errors) message) :to-equal
                "Else clause will never be executed.")))

    (it "else should not be reachable if the condition has non-nullable type"
      (elsa-test-with-buffer "|(if out 1 2)"
        (let ((scope (elsa-scope)))
          (elsa-scope-add-variable scope (elsa-variable :name 'out :type (elsa-type-int)))
          (elsa-infer-types (elsa-read-form) scope (elsa-scope))
          (expect (length elsa-infer-errors) :to-be 1)
          (expect (oref (car elsa-infer-errors) message) :to-equal
                  "Else clause will never be executed."))))

    (it "should not produce any warning for nullable type"
      (elsa-test-with-buffer "|(if out 1 2)"
        (let ((scope (elsa-scope)))
          (elsa-scope-add-variable
           scope (elsa-variable :name 'out :type (elsa-make-type 'mixed)))
          (elsa-infer-types (elsa-read-form) scope (elsa-scope))
          (expect (length elsa-infer-errors) :to-be 0)))))

  (describe "Unnecessary progn"

    (it "should complain if there is a progn wrapping else body"
      (elsa-test-with-buffer "|(if out 1 (progn 2))"
        (let ((scope (elsa-scope)))
          (elsa-scope-add-variable
           scope (elsa-variable :name 'out :type (elsa-make-type 'mixed)))
          (elsa-infer-types (elsa-read-form) scope (elsa-scope))
          (expect (length elsa-infer-errors) :to-be 1)
          (expect (oref (car elsa-infer-errors) message) :to-equal
                  "Unnecessary `progn' wrapping else branch."))))))
