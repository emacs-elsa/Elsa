;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa-reader)

(require 'elsa-test-helpers)

(describe "Elsa reader"

  (describe "symbol"

    (it "should read a symbol"
      (elsa-test-with-buffer "| foo"
        (let ((form (elsa-read-form)))
          (expect (elsa-form-symbol-p form) :to-be-truthy)
          (expect (oref form name) :to-be 'foo)
          (expect (oref form start) :to-be 2)
          (expect (oref form end) :to-be 5))))))
