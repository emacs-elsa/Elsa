;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-analyser)

(describe "Form analysis"

  (describe "eq"

    (describe "return type analysis"

      (describe "with one argument being variable"

        (it "should return bool if it is maybe eq or not-eq to the other arg"
          (let ((state (elsa-state)))
            (elsa-scope-add-var (oref state scope)
              (elsa-variable :name 'max
                             :type (elsa-type-mixed)))
            (elsa-test-with-analysed-form "(eq max 'undefined)" form
              :state state
              (expect form :to-be-type-equivalent (elsa-type-bool)))))

        (it "should return t if it is surely eq to the other arg"
          (let ((state (elsa-state)))
            (elsa-scope-add-var (oref state scope)
              (elsa-variable :name 'max
                             :type (elsa-make-type (const undefined))))
            (elsa-test-with-analysed-form "(eq max 'undefined)" form
              :state state
              (expect form :to-be-type-equivalent (elsa-type-t)))))

        (it "should return nil if it is surely not eq to the other arg"
          (let ((state (elsa-state)))
            (elsa-scope-add-var (oref state scope)
              (elsa-variable :name 'max
                             :type (elsa-type-string)))
            (elsa-test-with-analysed-form "(eq max 'undefined)" form
              :state state
              (expect form :to-be-type-equivalent (elsa-type-nil)))))))

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
