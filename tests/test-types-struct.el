;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-state)
(require 'elsa-types)

(describe "Elsa Types"

  (describe "elsa-struct-type"

    :var (state)

    (before-each
      (setq state (elsa-state))
      (elsa-state-add-structure state
        (elsa-cl-structure
         :name 'elsa-form-symbol
         :slots '()
         :parents '((elsa-form-symbol))))
      (elsa-state-add-structure state
        (elsa-cl-structure
         :name 'elsa-form-keyword
         :slots '()
         :parents '((elsa-form-keyword elsa-form-symbol)
                    (elsa-form-symbol)))))

    (describe "Subtype rules"
      (it "type should respect subtype relation of the structure"
        (expect (elsa-make-type (struct elsa-form-symbol)) :to-accept-type
                (elsa-make-type (struct elsa-form-keyword))))

      (it "mixed without a supertype should not accept subtype"
        (expect (elsa-make-type (diff mixed (struct elsa-form-symbol))) :not :to-accept-type
                (elsa-make-type (struct elsa-form-keyword))))

      (it "mixed without a subtype should not accept supertype"
        (expect (elsa-make-type (diff mixed (struct elsa-form-keyword))) :not :to-accept-type
                (elsa-make-type (struct elsa-form-symbol)))))

    (describe "Could accept rules"
      (it "mixed without a supertype could never accept subtype"
        (expect (trinary-false-p
                 (elsa-type-could-accept
                  (elsa-make-type (diff mixed (struct elsa-form-symbol)))
                  (elsa-make-type (struct elsa-form-keyword))))))

      (it "mixed without a subtype could accept supertype"
        (expect (trinary-maybe-p
                 (elsa-type-could-accept
                  (elsa-make-type (diff mixed (struct elsa-form-keyword)))
                  (elsa-make-type (struct elsa-form-symbol)))))))

    (xdescribe "Methods"

      (xdescribe "elsa-type-get-args")

      (xdescribe "elsa-type-get-return")

      (xdescribe "elsa-type-composite-p")

      (xdescribe "elsa-type-callable-p")

      (xdescribe "elsa-function-type-nth-arg"))

    (xdescribe "Sum rules")

    (xdescribe "Intersection rules")

    (xdescribe "Diff rules")))
