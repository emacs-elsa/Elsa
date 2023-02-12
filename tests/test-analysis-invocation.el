;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-analyser)

(describe "elsa--analyse-function-like-invocation"

  (describe "overloads"

    (it "should resolve to only one overload based on the matching argument"
      (let ((state (elsa-state)))
        (elsa-state-add-defun state 'b (elsa-make-type
                                        (and (function (vector) vector)
                                             (function (sequence) sequence))))
        (elsa-test-with-analysed-form "|(b [asd])" form
          :state state
          (expect form :to-be-type-equivalent (elsa-make-type vector))))))


  (describe "resolving expression type of funcall with type guard"

    (it "should resolve the type of expression to t if the type matches the guard"
      (elsa-test-with-analysed-form "|(integerp 1)" form
        (expect form :to-be-type-equivalent (elsa-make-type t))))

    (it "should resolve the type of expression to nil if the type can never match the guard"
      (elsa-test-with-analysed-form "|(integerp :keyword)" form
        (expect form :to-be-type-equivalent (elsa-make-type nil))))

    (it "should resolve the type of expression to bool if we don't know"
      (let ((state (elsa-state)))
        (elsa-state-add-defvar state 'a (elsa-make-type mixed))
        (elsa-test-with-analysed-form "|(integerp a)" form
          :state state
          (expect form :to-be-type-equivalent (elsa-make-type bool))))))

  (describe "resolving narrowed type of variable in a predicate"

    (it "should narrow the type of a variable to the predicated type"
      (let ((state (elsa-state)))
        (elsa-state-add-defvar state 'a (elsa-make-type mixed))
        (elsa-test-with-analysed-form "|(and (integerp a) a)" form
          :state state
          (expect (elsa-nth 2 form) :to-be-type-equivalent (elsa-make-type int)))))))
