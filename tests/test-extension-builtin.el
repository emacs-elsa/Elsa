;; -*- lexical-binding: t -*-

(require 'elsa-analyser)
(require 'elsa-extension-builtin)

(require 'elsa-test-helpers)

(describe "Elsa analyser"

  (describe "when"

    (describe "return type analysis"

      (it "should return true-body type if condition is always true"
        (elsa-test-with-analysed-form "|(when t 1)" form
          (expect form :to-be-type-equivalent (elsa-type-int))))

      (it "should return nil if condition is always false"
        (elsa-test-with-analysed-form "|(when nil 1)" form
          (expect form :to-be-type-equivalent (elsa-type-nil))))

      (it "should return nullable return type of true-body if condition is neither always true nor false."
        (elsa-test-with-analysed-form "|(defun a (x) (when x 1))" form
          (let ((test-form (elsa-nth 3 form)))
            (expect test-form :to-be-type-equivalent (elsa-make-type Int?))))))

    (describe "narrowing types"

      (it "should narrow a variable to its type in body"
        (elsa-test-with-analysed-form "|(defun fn (x) (when x x))" form
          (let ((second-cond (elsa-nth 2 (elsa-nth 3 form))))
            (expect (oref second-cond type) :to-be-type-equivalent
                    (elsa-type-diff (elsa-type-mixed) (elsa-type-nil))))))

      (it "should restore the variable type after the if body"
        (elsa-test-with-analysed-form "|(defun fn (x) (when x x) x)" form
          (let ((var-form (elsa-nth 4 form)))
            (expect (oref var-form type) :to-be-type-equivalent
                    (elsa-type-mixed)))))))

  (describe "unless"

    (describe "return type analysis"

      (it "should return true-body type if condition is always false"
        (elsa-test-with-analysed-form "|(unless nil 1)" form
          (expect form :to-be-type-equivalent (elsa-type-int))))

      (it "should return nil if condition is always true"
        (elsa-test-with-analysed-form "|(unless t 1)" form
          (expect form :to-be-type-equivalent (elsa-type-nil))))

      (it "should return nullable return type of true-body if condition is neither always true nor false."
        (elsa-test-with-analysed-form "|(defun a (x) (unless x 1))" form
          (let ((test-form (elsa-nth 3 form)))
            (expect test-form :to-be-type-equivalent (elsa-make-type Int?))))))

    (describe "narrowing types"

      (it "should narrow a variable to the complement of its type in body"
        (elsa-test-with-analysed-form "|(defun fn (x) (unless (stringp x) x))" form
          (let ((second-cond (elsa-nth 2 (elsa-nth 3 form))))
            (expect (oref second-cond type) :to-be-type-equivalent
                    (elsa-type-diff (elsa-type-mixed) (elsa-type-string))))))

      (it "should restore the variable type after the if body"
        (elsa-test-with-analysed-form "|(defun fn (x) (unless x x) x)" form
          (let ((var-form (elsa-nth 4 form)))
            (expect (oref var-form type) :to-be-type-equivalent
                    (elsa-type-mixed))))))))
