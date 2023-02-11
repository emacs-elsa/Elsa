;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-types)

(describe "Elsa Types Methods"

  (describe "elsa-type-callable-p"

    (it "function type is callable"
      (expect (elsa-type-callable-p (elsa-make-type (function () int))) :to-be-truthy))

    (it "string type is not callable"
      (expect (elsa-type-callable-p (elsa-make-type string)) :not :to-be-truthy))

    (it "empty sum type is not callable"
      (expect (elsa-type-callable-p (elsa-sum-type)) :not :to-be-truthy))

    (it "sum type with string and int is not callable"
      (expect (elsa-type-callable-p (elsa-make-type (or string int))) :not :to-be-truthy))

    (it "sum type with string and function is not callable"
      (expect (elsa-type-callable-p (elsa-make-type (or string (function () int)))) :not :to-be-truthy))

    (it "sum of two functions is callable"
      (expect (elsa-type-callable-p (elsa-make-type (or (function (int) int)
                                                        (function () int))))
              :to-be-truthy)))

  (describe "elsa-function-type-nth-arg"

    (it "for non-function type return nil"
      (expect (elsa-function-type-nth-arg (elsa-make-type string) 1) :not :to-be-truthy))

    (it "for sum type with string and function return the function argument"
      (expect (elsa-function-type-nth-arg
               (elsa-make-type (or string (function (int) int))) 0)
              :to-be-type-equivalent (elsa-make-type int)))

    (it "for sum type with two functions return the sum of function arguments"
      (expect (elsa-function-type-nth-arg
               (elsa-make-type (or (function (string) string)
                                   (function (int) int))) 0)
              :to-be-type-equivalent (elsa-make-type (or string int)))))

  (describe "elsa-type-get-return"

    (it "for non-function type return itself"
      (expect (elsa-type-get-return (elsa-make-type string))
              :to-be-type-equivalent (elsa-make-type string)))

    (it "for function type return the return type"
      (expect (elsa-type-get-return (elsa-make-type (function (int) symbol)))
              :to-be-type-equivalent (elsa-make-type symbol)))

    (it "for sum type return the sum of return types"
      (expect (elsa-type-get-return
               (elsa-make-type (or int (function (int) symbol))))
              :to-be-type-equivalent (elsa-make-type (or int symbol))))))
