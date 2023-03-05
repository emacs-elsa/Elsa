;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-state)
(require 'elsa-types)

(describe "Elsa Types"

  (describe "elsa-type-plist"

    (describe "Subtype rules"

      (it "plist can accept another plist with compatible slots"
        (expect (elsa-make-type (plist :a number))
                :to-accept-type (elsa-make-type (plist :a number :b keyword))))

      (it "plist can accept another plist which extends compatible interface"
        (unwind-protect
            (progn
              (put 'has-a 'elsa-interface (elsa-make-type (interface has-a :a number)))
              (expect (elsa-make-type (plist :a number))
                      :to-accept-type (elsa-make-type (plist &extends has-a))))
          (put 'has-a 'elsa-interface nil)))

      (it "plist can not accept another plist with missint slots"
        (expect (elsa-make-type (plist :a number :b keyword))
                :not :to-accept-type (elsa-make-type (plist :a number))))

      (it "plist can not accept another plist which extends smaller interface"
        (unwind-protect
            (progn
              (put 'has-a 'elsa-interface (elsa-make-type (interface has-a :a number)))
              (expect (elsa-make-type (plist :a number :b number))
                      :not :to-accept-type (elsa-make-type (plist &extends has-a))))
          (put 'has-a 'elsa-interface nil))))

    (xdescribe "Could accept rules")

    (describe "Methods"

      (describe "elsa-make-type"
        (it "can define ad-hoc plist by specifying slots"
          (let ((plist (elsa-make-type (plist :a number :b bool))))
            (expect (elsa-type-plist-p plist))
            (expect (oref (elsa-get-slot plist :a) type)
                    :to-be-type-equivalent (elsa-type-number))))

        (it "can define plist by only extending an interface"
          (unwind-protect
              (progn
                (put 'has-name 'elsa-interface (elsa-make-type (interface has-name :name keyword)))
                (let ((plist (elsa-make-type (plist &extends has-name))))
                  (expect (elsa-type-plist-p plist))
                  (expect (oref (elsa-get-slot plist :name) type)
                          :to-be-type-equivalent (elsa-type-keyword))))
            (put 'has-name 'elsa-interface nil)))

        (it "can define plist by having slots and extending an interface"
          (unwind-protect
              (progn
                (put 'has-name 'elsa-interface (elsa-make-type (interface has-name :name keyword)))
                (let ((plist (elsa-make-type (plist :slot number &extends has-name))))
                  (expect (elsa-type-plist-p plist))
                  (expect (oref (elsa-get-slot plist :name) type)
                          :to-be-type-equivalent (elsa-type-keyword))
                  (expect (oref (elsa-get-slot plist :slot) type)
                          :to-be-type-equivalent (elsa-type-number))))
            (put 'has-name 'elsa-interface nil))))

      (describe "elsa-type-describe"

        (it "should not use &extends if plist extends nothing"
          (expect (elsa-type-describe (elsa-make-type (plist :foo string :bar number)))
                  :to-equal "(plist :bar number :foo string)"))

        (it "should use &extends if plist extends some interface"
          (expect (elsa-type-describe (elsa-make-type (plist :foo string :bar number &extends b a)))
                  :to-equal "(plist :bar number :foo string &extends a b)")))

      (xdescribe "elsa-type-get-args")

      (xdescribe "elsa-type-get-return")

      (xdescribe "elsa-type-composite-p")

      (xdescribe "elsa-type-callable-p")

      (xdescribe "elsa-function-type-nth-arg"))

    (xdescribe "Sum rules")

    (xdescribe "Intersection rules")

    (xdescribe "Diff rules")

    (xdescribe "Type normalization rules")))
