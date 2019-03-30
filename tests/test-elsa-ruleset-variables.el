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
      (elsa-ruleset-load (elsa-ruleset-variables)))))
