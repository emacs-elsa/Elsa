;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa-reader)

(require 'elsa-test-helpers)

(describe "Elsa reader"

  (describe "symbol"

    (it "should read a symbol"
      (elsa-test-with-buffer "| foo"
        (let ((form (elsa-read-form)))
          (expect (elsa-form-symbol-p form) :to-be-truthy)
          (expect (oref form name) :to-be 'foo)
          (expect (oref form start) :to-be 2)
          (expect (oref form end) :to-be 5)))))

  (describe "lists"
    (it "should read nested lists"
      (elsa-test-with-buffer "|((foo))"
        (let ((form (elsa-read-form)))
          (expect (elsa-form-list-p form) :to-be-truthy)))))

  (describe "quotes"

    (it "should read a quoted symbol"
      (elsa-test-with-buffer "|'foo"
        (let ((form (elsa-read-form)))
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-name form) :to-be 'quote)
          (expect (oref form quote-type) :to-be 'quote))))

    (it "should read an unquoted symbol"
      (elsa-test-with-buffer "|,foo"
        (let ((form (elsa-read-form)))
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-name form) :to-be '\,)
          (expect (oref form quote-type) :to-be '\,))))

    (it "should read a backquoted symbol"
      (elsa-test-with-buffer "|`foo"
        (let ((form (elsa-read-form)))
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-name form) :to-be '\`)
          (expect (oref form quote-type) :to-be '\`))))

    (it "should read a spliced symbol"
      (elsa-test-with-buffer "|,@foo"
        (let ((form (elsa-read-form)))
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-name form) :to-be '\,@)
          (expect (oref form quote-type) :to-be '\,@))))

    (it "should read a list containing quoted nil"
      (elsa-test-with-buffer "|(foo 'nil)"
        (let ((form (elsa-read-form)))
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-list-p (cadr (oref form sequence))) :to-be-truthy))))))
