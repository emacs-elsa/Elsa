;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-state)
(require 'elsa-types)

(describe "Elsa Types"

  (describe "elsa-type-symbol"

    (describe "Subtype rules"

      (it "should recognize keyword as subtype"
        (expect (elsa-type-symbol) :to-accept-type (elsa-type-keyword)))

      (it "should accept nil"
        (expect (elsa-type-symbol) :to-accept-type (elsa-type-nil)))

      (it "should accept t"
        (expect (elsa-type-symbol) :to-accept-type (elsa-type-t))))

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
