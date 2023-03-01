;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-state)
(require 'elsa-types)

(describe "Elsa Types"

  (describe "elsa-diff-type"

    (describe "Subtype rules"

      (it "mixed without nil should accept itself"
        (expect
         (elsa-make-type (diff mixed nil))
         :to-accept-type
         (elsa-make-type (diff mixed nil))))

      (it "mixed without int should accept itself"
        (expect
         (elsa-make-type (diff mixed int))
         :to-accept-type
          (elsa-make-type (diff mixed int)))))

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
