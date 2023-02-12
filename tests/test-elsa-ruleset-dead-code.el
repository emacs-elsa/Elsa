;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa)
(require 'elsa-reader)
(require 'elsa-types)
(require 'elsa-extension-builtin)
(require 'elsa-ruleset)
(require 'trinary)

(describe "Ruleset"

  (describe "Dead code"

    (before-all
      (setq elsa-checks nil)
      (elsa-ruleset-load (elsa-ruleset-dead-code)))

    (describe "If"

      (it "then should not be reachable if the condition is nil"
        (elsa-test-with-analysed-form "|(if nil 1 2)" form
          :state-var state
          (expect (oref state errors) :not :to-be nil)
          (expect (oref (car (oref state errors)) message) :to-equal
                  "Condition always evaluates to nil.")))

      (it "else should not be reachable if the condition is t"
        (elsa-test-with-analysed-form "|(if t 1 2)" form
          :state-var state
          (expect (oref state errors) :not :to-be nil)
          (expect (oref (car (oref state errors)) message) :to-equal
                  "Condition always evaluates to non-nil.")))

      (it "else should not be reachable if the condition is atom"
        (elsa-test-with-analysed-form "|(if \"string\" 1 2)" form
          :state-var state
          (expect (oref state errors) :not :to-be nil)
          (expect (oref (car (oref state errors)) message) :to-equal
                  "Condition always evaluates to non-nil.")))

      (it "else should not be reachable if the condition has non-nullable type"
        (elsa-test-with-analysed-form "|(let ((out 1)) (if out 1 2))" form
          :state-var state
          (expect (oref state errors) :not :to-be nil)
          (expect (oref (car (oref state errors)) message) :to-equal
                  "Condition always evaluates to non-nil.")))

      (it "should not produce any warning for nullable type"
        (elsa-test-with-analysed-form "|(defun foo (out) (if out 1 2))" form
          :state-var state
          (expect (oref state errors) :to-be nil))))

    (describe "unreachable code determined by state"

      (it "should report unreachable state as tracked by the state"
        (elsa-test-with-analysed-form "|(cond ((integerp 1) \"yes\") ((current-time) \"whatever\"))" form
          :errors-var errors
          (expect (trinary-false-p (oref (elsa-car (elsa-nth 2 form)) reachable)) :to-be t)
          (expect (cadr errors) :message-to-match "Unreachable expression (current-time)"))))))
