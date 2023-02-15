;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-state)
(require 'elsa-types)

(xdescribe "Elsa Types"

  (xdescribe "elsa-struct-type"

    (xdescribe "Subtype rules")

    (xdescribe "Could accept rules")

    (xdescribe "Methods"

      (xdescribe "elsa-type-get-args")

      (xdescribe "elsa-type-get-return")

      (xdescribe "elsa-type-composite-p")

      (xdescribe "elsa-type-callable-p")

      (xdescribe "elsa-function-type-nth-arg"))

    (xdescribe "Sum rules")

    (xdescribe "Intersection rules")

    (xdescribe "Diff rules")

    (xdescribe "Type normalization rules")))
