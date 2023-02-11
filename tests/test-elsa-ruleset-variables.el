;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-reader)
(require 'elsa-types)
(require 'elsa-extension-builtin)
(require 'elsa-ruleset)

(describe "Ruleset"

  (describe "Variables"

    (before-all
      (setq elsa-checks nil)
      (elsa-ruleset-load (elsa-ruleset-variables)))))
