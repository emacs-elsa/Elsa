;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-analyser)
(require 'elsa-extension-builtin)

(describe "Elsa analyser - builtin"

  (describe "eq"

    (describe "return type analysis"

      (it "should return nil if intersection of domains of arguments is nil"
        (elsa-test-with-analysed-form "|(eq :keyword 1)" form
          (expect form :to-be-type-equivalent (elsa-type-nil)))))

    (describe "narrowing types"

      (it "should narrow a variable to the type of t constant in first argument"
        (elsa-test-with-analysed-form "|(defun fn (x) (eq t x))" form
          (let ((eq-form (elsa-nth 3 form)))
            (expect (car (oref eq-form narrow-types)) :to-be-type-equivalent
                    (elsa-make-type t)))))

      (it "should narrow a variable to the type of t constant in second argument"
        (elsa-test-with-analysed-form "|(defun fn (x) (eq x t))" form
          (let ((eq-form (elsa-nth 3 form)))
            (expect (car (oref eq-form narrow-types)) :to-be-type-equivalent
                    (elsa-make-type t)))))

      (it "should narrow a variable to the type of keyword in second argument"
        (elsa-test-with-analysed-form "|(defun fn (x) (eq x :keyword))" form
          (let ((eq-form (elsa-nth 3 form)))
            (expect (car (oref eq-form narrow-types)) :to-be-type-equivalent
                    (elsa-make-type (const :keyword))))))

      (it "should narrow a variable to the type of symbol in second argument"
        (elsa-test-with-analysed-form "|(defun fn (x) (eq x 'symbol))" form
          (let ((eq-form (elsa-nth 3 form)))
            (expect (car (oref eq-form narrow-types)) :to-be-type-equivalent
                    (elsa-make-type (const symbol))))))

      (it "should narrow a variable to the type of int in second argument"
        (elsa-test-with-analysed-form "|(defun fn (x) (eq x 1))" form
          (let ((eq-form (elsa-nth 3 form)))
            (expect (car (oref eq-form narrow-types)) :to-be-type-equivalent
                    (elsa-make-type (const 1))))))

      (it "should narrow a variable to the type of float in second argument"
        (elsa-test-with-analysed-form "|(defun fn (x) (eq x 1.1))" form
          (let ((eq-form (elsa-nth 3 form)))
            (expect (car (oref eq-form narrow-types)) :to-be-type-equivalent
                    (elsa-make-type (const 1.1))))))))

  (describe "cons"

    (describe "return type analysis"

      (it "should set car and cdr types to constant types of the args"
        (elsa-test-with-analysed-form "(cons 1 :keyword)" form
          (expect form :to-be-type-equivalent
                  (elsa-make-type (cons (const 1) (const :keyword))))))

      (it "should set car and cdr types to return types of the args"
        (elsa-test-with-analysed-form "(cons (point) (bobp))" form
          (expect form :to-be-type-equivalent
                  (elsa-make-type (cons int bool)))))))

  (describe "when"

    (describe "return type analysis"

      (it "should return true-body type if condition is always true"
        (elsa-test-with-analysed-form "|(when t 1)" form
          (expect form :to-be-type-equivalent (elsa-make-type (const 1)))))

      (it "should return nil if condition is always false"
        (elsa-test-with-analysed-form "|(when nil 1)" form
          (expect form :to-be-type-equivalent (elsa-type-nil))))

      (it "should return nullable return type of true-body if condition is neither always true nor false."
        (elsa-test-with-analysed-form "|(defun a (x) (when x 1))" form
          (let ((test-form (elsa-nth 3 form)))
            (expect test-form :to-be-type-equivalent (elsa-make-type (or nil (const 1))))))))

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
          (expect form :to-be-type-equivalent (elsa-make-type (const 1)))))

      (it "should return nil if condition is always true"
        (elsa-test-with-analysed-form "|(unless t 1)" form
          (expect form :to-be-type-equivalent (elsa-type-nil))))

      (it "should return nullable return type of true-body if condition is neither always true nor false."
        (elsa-test-with-analysed-form "|(defun a (x) (unless x 1))" form
          (let ((test-form (elsa-nth 3 form)))
            (expect test-form :to-be-type-equivalent (elsa-make-type (or nil (const 1))))))))

    (describe "narrowing types"

      (it "should narrow a variable to the complement of its type in body"
        (elsa-test-with-analysed-form "|(defun fn (x) (unless (stringp x) x))" form
          (let ((second-cond (elsa-nth 2 (elsa-nth 3 form))))
            (expect (oref second-cond type) :to-be-type-equivalent
                    (elsa-type-diff (elsa-type-mixed) (elsa-type-string))))))

      (it "should restore the variable type after the unless body"
        (elsa-test-with-analysed-form "|(defun fn (x) (unless x x) x)" form
          (let ((var-form (elsa-nth 4 form)))
            (expect (oref var-form type) :to-be-type-equivalent
                    (elsa-type-mixed))))))))
