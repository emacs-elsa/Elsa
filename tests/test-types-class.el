;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-state)
(require 'elsa-types)

(describe "Elsa Types"

  (describe "elsa-class-type"

    :var (state)

    (before-each
      (setq state (elsa-state))
      (elsa-state-add-defclass state
        (elsa-defclass
         :name 'elsa-form-symbol
         :slots (let ((ht (make-hash-table)))
                  (puthash 'name (elsa-structure-slot
                                  :name 'name
                                  :type (elsa-type-symbol))
                           ht)
                  ht)
         :parents '((elsa-form-symbol))))
      (elsa-state-add-defclass state
        (elsa-defclass
         :name 'elsa-form-keyword
         :slots (let ((ht (make-hash-table)))
                  (puthash 'is-keyword (elsa-structure-slot
                                        :name 'is-keyword
                                        :type (elsa-type-bool))
                           ht)
                  ht)
         :parents '((elsa-form-keyword elsa-form-symbol)
                    (elsa-form-symbol)))))

    (describe "Subtype rules"

      (describe "class declared as class"

        (describe "with other class"
          (it "type should respect subtype relation of the class"
            (expect (elsa-make-type (class elsa-form-symbol)) :to-accept-type
                    (elsa-make-type (class elsa-form-keyword))))

          (it "mixed without a supertype should not accept subtype"
            (expect (elsa-make-type (diff mixed (class elsa-form-symbol))) :not :to-accept-type
                    (elsa-make-type (class elsa-form-keyword))))

          (it "mixed without a subtype should not accept supertype"
            (expect (elsa-make-type (diff mixed (class elsa-form-keyword))) :not :to-accept-type
                    (elsa-make-type (class elsa-form-symbol)))))

        (describe "with interface"
          (xit "should accept interface which contains all the class slots"
            (let ((cls (elsa-make-type (class elsa-form-symbol)))
                  (iface (elsa-make-type (interface has-name name keyword)) ))
              (expect cls :to-accept-type iface)))

          (it "should accept class defined by an interface"
            (unwind-protect
                (progn
                  (put 'has-name 'elsa-interface (elsa-make-type (interface has-name name keyword)))
                  (let ((cls (elsa-make-type (class elsa-form-symbol)))
                        (iface (elsa-make-type (class has-name))))
                    (expect cls :to-accept-type iface)))
              (put 'has-name 'elsa-interface nil)))))

      (xdescribe "class declared as interface"

        (describe "with interface"

          (it "should accept class defined by an interface with all the slots"
            (unwind-protect
                (progn
                  (put 'has-name 'elsa-interface (elsa-make-type (interface has-name name keyword)))
                  (put 'has-name-2 'elsa-interface (elsa-make-type (interface has-name-2 name keyword is-fancy bool)))
                  (let ((this-cls (elsa-make-type (class has-name)))
                        (other-cls (elsa-make-type (class has-name-2))))
                    (expect this-cls :to-accept-type other-cls)))
              (put 'has-name 'elsa-interface nil)
              (put 'has-name-2 'elsa-interface nil)))

          (it "should not accept class defined by an interface with missing slots"
            (unwind-protect
                (progn
                  (put 'has-name 'elsa-interface (elsa-make-type (interface has-name name keyword)))
                  (put 'has-name-2 'elsa-interface (elsa-make-type (interface has-name-2 name keyword is-fancy bool)))
                  (let ((this-cls (elsa-make-type (class has-name-2)))
                        (other-cls (elsa-make-type (class has-name))))
                    (expect this-cls :not :to-accept-type other-cls)))
              (put 'has-name 'elsa-interface nil)
              (put 'has-name-2 'elsa-interface nil))))))

    (describe "Could accept rules"
      (it "mixed without a supertype could never accept subtype"
        (expect (trinary-false-p
                 (elsa-type-could-accept
                  (elsa-make-type (diff mixed (class elsa-form-symbol)))
                  (elsa-make-type (class elsa-form-keyword))))))

      (it "mixed without a subtype could accept supertype"
        (expect (trinary-maybe-p
                 (elsa-type-could-accept
                  (elsa-make-type (diff mixed (class elsa-form-keyword)))
                  (elsa-make-type (class elsa-form-symbol)))))))

    (describe "Methods"

      (describe "elsa-get-slots"
        (it "should get slots from this and parents"
          (let ((slots (elsa-get-slots (elsa-make-type (class elsa-form-keyword)))))
            (expect (length slots) :to-be 2)
            (expect (memq 'name (mapcar (lambda (s) (oref s name)) slots)))
            (expect (memq 'is-keyword (mapcar (lambda (s) (oref s name)) slots))))))

      (describe "elsa-get-slot"

        (it "should get slot from this class"
          (let ((slot (elsa-get-slot (elsa-make-type (class elsa-form-keyword)) 'is-keyword)))
            (expect (oref slot name) :to-be 'is-keyword)))

        (it "should get slot from parent class"
          (let ((slot (elsa-get-slot (elsa-make-type (class elsa-form-keyword)) 'name)))
            (expect (oref slot name) :to-be 'name))))

      (xdescribe "elsa-type-get-args")

      (xdescribe "elsa-type-get-return")

      (xdescribe "elsa-type-composite-p")

      (xdescribe "elsa-type-callable-p")

      (xdescribe "elsa-function-type-nth-arg"))

    (xdescribe "Sum rules")

    (xdescribe "Intersection rules")

    (xdescribe "Diff rules")))
