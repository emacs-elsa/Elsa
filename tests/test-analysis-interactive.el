;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-analyser)

(describe "elsa--analyse:interactive"

  (it "should parse interactive spec with trailing newline"
    (elsa-test-with-analysed-form "|(interactive \"MURL of the file to attach: \n\")" form
      :errors-var errors
      (expect errors :to-be nil)))

  (it "should throw error when the interactive spec is invalid"
    (elsa-test-with-analysed-form "|(interactive \"AURL\")" form
      :errors-var errors
      (expect (car errors) :message-to-match "Unknown interactive code letter"))))
