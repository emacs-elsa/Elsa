;; -*- lexical-binding: t -*-

(require 'elsa-analyser)

(require 'elsa-test-helpers)

(describe "Elsa analyser - defun"

  (describe "return type"

    (it "Should read function argument types from declaration."
      (elsa-test-with-analysed-form ";; (xxx :: (function (string) string))
(defun xxx (a) a)"
          form
        (expect (elsa-nth 3 form) :to-be-type-equivalent (elsa-make-type string))))

    (it "Should construct nullary function type if only symbol is probided as type"
      (elsa-test-with-analysed-form ";; (xxx :: string)
(defun xxx () \"foo\")"
          form
        (expect (get 'xxx 'elsa-type) :to-be-type-equivalent (elsa-make-type (function () string))))))

  (describe "argument type"

    (it "should derive nth argument type of sum of functions as sum of nth argument"
      (elsa-test-with-analysed-form ";; (xxx :: (or (function (string) string) (function (int) int)))
(defun xxx (a) a)"
          form
        (expect (elsa-nth 3 form) :to-be-type-equivalent (elsa-make-type (or string int)))))))
