;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-reader)
(require 'elsa-types)
(require 'elsa-extension-builtin)
(require 'elsa-ruleset)

(describe "Ruleset"

  (describe "Functions"

    (before-all
      (setq elsa-checks nil)
      (elsa-ruleset-load (elsa-ruleset-functions)))

    (describe "elsa-check-useless-predicate"

      (it "should warn if the type-testing predicate always returns true"
        (elsa-test-with-analysed-form "|(when (stringp \"foo\") :yay)" form
          :errors-var errors
          (expect (length errors) :to-be-greater-than 0)
          (expect (car errors) :message-to-match
                  "Function `stringp' narrows argument type to `string' and the argument type is `(const \"foo\")'\n└─ Expression always evaluates to true because the argument \"foo\" will always be `string'")))

      (it "should warn if the type-testing predicate never returns true"
        (elsa-test-with-analysed-form "|(when (stringp 1) :yay)" form
          :errors-var errors
          (expect (length errors) :to-be-greater-than 0)
          (expect (oref (car errors) message) :to-match
                  "Function `stringp' narrows argument type to `string' and the argument type is `(const 1)'.\n  Expression always evaluates to false because the argument 1 can never be `string'")))

      (it "should not warn if the type-testing predicate could return true or false"
        (let ((state (elsa-state)))
          (elsa-state-add-defvar state (elsa-defvar :name 'a :type (elsa-make-type (or string int))))
          (elsa-test-with-analysed-form "|(when (stringp a) :yay)" form
            :state state
            :errors-var errors
            (expect (length errors) :to-be 0)))))))
