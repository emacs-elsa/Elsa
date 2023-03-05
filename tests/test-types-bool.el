;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-state)
(require 'elsa-types)

(describe "Elsa Types"

  (describe "elsa-type-bool"

    (describe "Subtype rules"

      (it "should accept t"
        (expect (elsa-type-bool) :to-accept-type (elsa-type-t)))

      (it "should accept nil"
        (expect (elsa-type-bool) :to-accept-type (elsa-type-nil)))

      (it "should accept bool"
        (expect (elsa-type-bool) :to-accept-type (elsa-type-bool))))

    (describe "Could accept rules"

      (it "bool could accept list which could be empty"
        (expect (trinary-maybe-p (elsa-type-could-accept (elsa-type-bool) (elsa-type-list)))))

      (it "nil could accept list which could be empty"
        (expect (trinary-maybe-p (elsa-type-could-accept (elsa-type-nil) (elsa-type-list))))))

    (xdescribe "Methods"

      (xdescribe "elsa-type-get-args")

      (xdescribe "elsa-type-get-return")

      (xdescribe "elsa-type-composite-p")

      (xdescribe "elsa-type-callable-p")

      (xdescribe "elsa-function-type-nth-arg"))

    (describe "Sum rules"

      (it "should sum t and nil to bool"
        (expect
         (elsa-type-bool-p (elsa-type-sum (elsa-type-t) (elsa-type-nil)))
         :to-be t)))

    (describe "Intersection rules"

      (it "bool intersected with list is nil"
        (expect (elsa-type-intersect (elsa-type-bool) (elsa-type-list))
                :to-be-type-equivalent (elsa-type-nil)))

      (it "nil intersected with list is nil"
        (expect (elsa-type-intersect (elsa-type-nil) (elsa-type-list))
                :to-be-type-equivalent (elsa-type-nil))))

    (describe "Diff rules"

      (it "t without t is empty"
        (expect (elsa-type-diff (elsa-type-t) (elsa-type-t))
                :to-be-type-equivalent (elsa-type-empty)))

      (it "nil without nil is empty"
        (expect (elsa-type-diff (elsa-type-nil) (elsa-type-nil))
                :to-be-type-equivalent (elsa-type-empty)))

      (it "t without nil is t"
        (expect (elsa-type-diff (elsa-type-t) (elsa-type-nil))
                :to-be-type-equivalent (elsa-type-t)))

      (it "nil without t is nil"
        (expect (elsa-type-diff (elsa-type-nil) (elsa-type-t))
                :to-be-type-equivalent (elsa-type-nil)))

      (it "bool without t is nil"
        (expect (elsa-type-diff (elsa-type-bool) (elsa-type-t))
                :to-be-type-equivalent (elsa-type-nil)))

      (it "bool without nil is t"
        (expect (elsa-type-diff (elsa-type-bool) (elsa-type-nil))
                :to-be-type-equivalent (elsa-type-t)))

      (it "bool without bool is empty"
        (expect (elsa-type-diff (elsa-type-bool) (elsa-type-bool))
                :to-be-type-equivalent (elsa-type-empty)))

      (it "t without bool is empty"
        (expect (elsa-type-diff (elsa-type-t) (elsa-type-bool))
                :to-be-type-equivalent (elsa-type-empty)))

      (it "nil without bool is empty"
        (expect (elsa-type-diff (elsa-type-nil) (elsa-type-bool))
                :to-be-type-equivalent (elsa-type-empty))))

    (describe "Type normalization rules"

      (it "should normalize sum of t and nil to bool"
        (expect (elsa-type-bool-p (elsa-type-normalize (elsa-make-type (or nil t))))
                :to-be t)))))
