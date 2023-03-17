;; -*- lexical-binding: t -*-
(require 'buttercup)

(require 'elsa-test-helpers)
(require 'elsa-analyser)

(describe "Form analysis"

  (describe "let"

    (xdescribe "return type analysis")

    (describe "introducing variables to scope"

      (it "should update local scope with let-bound variables"
        (let ((state (elsa-state)))
          (spy-on 'elsa-scope-add-var :and-call-through)
          (elsa-test-with-analysed-form "(let ((slot 1)))" form
            :state state
            (expect 'elsa-scope-add-var
                    :to-have-been-called-with
                    (spy-arg-matcher #'elsa-scope-p)
                    (spy-arg-matcher
                     (lambda (x)
                       (and (elsa-variable-p x)
                            (eq (oref x name) 'slot)
                            (elsa-type-accept (elsa-type-int) (oref x type))))))))))

    (describe "annotations"

      (it "should set type of binding to type from annotation"
        (elsa-test-with-analysed-form ";; (slot :: number)\n(let ((slot (mixed))) slot)"
            form
          (expect (elsa-nth 2 form)
                  :to-accept-type (elsa-type-number)))))))
