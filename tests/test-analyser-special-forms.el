;; -*- lexical-binding: t -*-

(require 'elsa-analyser)
(require 'elsa-types)

(require 'elsa-test-helpers)

(describe "Elsa analyser"

  (describe "Special forms"

    (describe "quote"

      (it "should resolve to type list if the quoted argument is a list"
        (elsa-test-with-analysed-form "'(foo)" form
          (expect (elsa-type-list-p (oref form type)) :to-be-truthy))))))
