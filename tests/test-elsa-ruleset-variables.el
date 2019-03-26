;; -*- lexical-binding: t -*-

(require 'elsa-reader)
(require 'elsa-types)
(require 'elsa-extension-builtin)
(require 'elsa-ruleset)

(require 'elsa-test-helpers)

(describe "Ruleset"

  (describe "Variables"

    (before-all
      (setq elsa-checks nil)
      (elsa-ruleset-load (elsa-ruleset-variables)))

    (describe "defconst"

      (it "should warn on assignment"
        (elsa-test-with-analysed-form "|(progn (defconst foo 'bar) (setq foo 'bar))" form
          :state-var state
          (expect (oref state errors) :not :to-be nil)
          (expect (oref (car (oref state errors)) message) :to-equal
                  "Assignment to defconst foo."))))))
