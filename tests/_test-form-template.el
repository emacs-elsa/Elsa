;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-analyser)

(xdescribe "Form analysis"

  (xdescribe "and"

    (xdescribe "return type analysis")

    (xdescribe "narrowing"

      (xdescribe "inside the form arglist")

      (xdescribe "in the then body"

        (xdescribe "when reachable")

        (xdescribe "when unreachable"))

      (xdescribe "in the else body"

        (xdescribe "when reachable")

        (xdescribe "when unreachable")))

    ;; for special forms which do not evaluate all arguments
    (xdescribe "argument reachability")

    ))
