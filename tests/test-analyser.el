;; -*- lexical-binding: t -*-

(require 'elsa-analyser)

(require 'elsa-test-helpers)

(describe "Elsa analyser"

  (describe "Normalize spec"

    (it "should evaluate all arguments when the spec is t"
      (elsa-test-with-analysed-form "|(fun a b c d)" form
        (expect (elsa--analyse-normalize-spec t form) :to-equal (list t t t t))))

    (it "should evaluate all remaining arguments when the spec ends in 'body"
      (elsa-test-with-analysed-form "|(fun a b c d)" form
        (expect (elsa--analyse-normalize-spec (list nil 'body) form) :to-equal (list nil t t t))))

    (it "should keep the spec as provided otherwise"
      (elsa-test-with-analysed-form "|(fun a b c d)" form
        (expect (elsa--analyse-normalize-spec (list nil t nil t) form) :to-equal (list nil t nil t)))))


  (describe "Narrowing types in if"


    (it "should narrow a variable to its type in true body"
      (elsa-test-with-analysed-form "|(defun fn (x) (if x x))" form
        (let ((second-cond (elsa-nth 2 (elsa-nth 3 form))))
          (expect (oref second-cond type) :to-be-type-equivalent
                  (elsa-type-diff (elsa-type-mixed) (elsa-type-nil))))))

    (it "should narrow a variable to nil in else body"
      (elsa-test-with-analysed-form "|(defun fn (x) (if x x x))" form
        (let ((second-cond (elsa-nth 3 (elsa-nth 3 form))))
          (expect (oref second-cond type) :to-be-type-equivalent
                  (elsa-type-nil)))))))
