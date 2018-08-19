;; -*- lexical-binding: t -*-

(require 'elsa-analyser)

(require 'elsa-test-helpers)

(describe "Elsa analyser"

  (describe "and"

    (it "should set return type to nil if we empty domain of some variable"
      (elsa-test-with-analysed-form "|(defun a (x) (and (stringp x) (integerp x)))" form
        (let ((and-form (elsa-nth 3 form)))
          (expect and-form :to-be-type-equivalent (elsa-type-nil)))))

    (it "should set return type to nil if there is a surely nil expression"
      (elsa-test-with-analysed-form "|(defun a (x) (and nil (1+ x)))" form
        (let ((and-form (elsa-nth 3 form)))
          (expect and-form :to-be-type-equivalent (elsa-type-nil)))))

    (it "should set return type to narrowed variable type"
      (elsa-test-with-analysed-form "|(defun a (x) (and (stringp x) x))" form
        (let ((and-form (elsa-nth 3 form)))
          (expect and-form :to-be-type-equivalent (elsa-make-type String?)))))

    (it "should set return type to t for empty (and) form"
      (elsa-test-with-analysed-form "|(and)" form
        (expect form :to-be-type-equivalent (elsa-type-t)))))

  (describe "or"

    (it "should set return type to nullable sum if there is no sure single branch."
      (elsa-test-with-analysed-form "|(defun a (x) (or (and (stringp x) x)))" form
        (let ((or-form (elsa-nth 3 form)))
          (expect or-form :to-be-type-equivalent
                  (elsa-make-type String?)))))

    (it "should set return type to nullable sum if there is no sure branch of multiple branches."
      (elsa-test-with-analysed-form "|(defun a (x) (or (and (stringp x) x) (and (integerp x) x)))" form
        (let ((or-form (elsa-nth 3 form)))
          (expect or-form :to-be-type-equivalent
                  (elsa-make-type String | Int | Nil)))))

    (it "should set return type to non-nullable sum if there is a sure branch."
      (elsa-test-with-analysed-form "|(defun a (x) (or (stringp x) 2 nil))" form
        (let ((or-form (elsa-nth 3 form)))
          (expect or-form :to-be-type-equivalent
                  (elsa-make-type T | Int)))))

    (it "should set return type to nil for empty (or) form"
      (elsa-test-with-analysed-form "|(or)" form
        (expect form :to-be-type-equivalent (elsa-type-nil)))))

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
