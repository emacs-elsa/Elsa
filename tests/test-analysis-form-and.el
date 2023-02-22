;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-analyser)

(describe "Form analysis"

  (describe "and"

    (describe "return type analysis")

    (describe "narrowing"

      (describe "inside the form arglist"

        (it "should narrow the type in the subsequent expressions of and form"
          (elsa-test-with-analysed-form "|(defun a (x) (and (stringp x) x))" form
            (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
              (expect test-form :to-be-type-equivalent (elsa-make-type string)))))


        )

      (describe "in the then body"

        (describe "when reachable"

          (it "should make variable non-nil"
            (elsa-test-with-analysed-form "|(defun a (x) (if (and x) x x))" form
              (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
                (expect test-form :to-be-type-equivalent
                        (elsa-type-make-non-nullable (elsa-make-type mixed)))))))

        (describe "when unreachable"

          (it "should not make variable non-nil"
            (elsa-test-with-analysed-form "|(defun a (x) (if (and nil x) x x))" form
              (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
                (expect test-form :to-be-type-equivalent (elsa-make-type mixed))))))
        )

      (describe "in the else body"

        (describe "when reachable"

          (it "should make variable nil"
            (elsa-test-with-analysed-form "|(defun a (x) (if (and x) x x))" form
              (let ((test-form (elsa-nth 3 (elsa-nth 3 form))))
                (expect test-form :to-be-type-equivalent (elsa-type-nil))))))

        (describe "when unreachable"

          (it "should not narrow the type by the unreachable expressions"
            (elsa-test-with-analysed-form "|(defun a (x) (if (and nil x) x x))" form
              (let ((test-form (elsa-nth 3 (elsa-nth 3 form))))
                (expect test-form :to-be-type-equivalent (elsa-make-type mixed))))))

        ))

    ;; for special forms which do not evaluate all arguments
    (describe "argument reachability")

    ))
