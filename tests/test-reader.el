;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-reader)

(describe "Elsa reader"

  (describe "symbols"

    (it "should read a symbol"
      (elsa-test-with-read-form "| foo" form
        (expect (elsa-form-symbol-p form) :to-be-truthy)
        (expect (oref form name) :to-be 'foo)
        (expect (oref form start) :to-be 2)
        (expect (oref form end) :to-be 5)
        (expect form :to-print-as "foo")))

    (it "should read a symbol composed of multiple dots"
      (elsa-test-with-read-form "| ..." form
        (expect (elsa-form-symbol-p form) :to-be-truthy)
        (expect (oref form name) :to-be '...)
        (expect (oref form start) :to-be 2)
        (expect (oref form end) :to-be 5)
        (expect form :to-print-as "..."))))

  (describe "improper lists"

    (it "should read a simple cons pair"
      (elsa-test-with-read-form "|(a . b)" form
        (expect (elsa-form-improper-list-p form) :to-be-truthy)
        (expect form :to-print-as "(a . b)")))

    (it "should read a simple cons pair with integers"
      (elsa-test-with-read-form "|(0 . 1)" form
        (expect (elsa-form-improper-list-p form) :to-be-truthy)
        (expect form :to-print-as "(0 . 1)")))

    (it "should read an improper list containgin a quote."
      (elsa-test-with-read-form "|(a . (quote . c))" form
        (expect (elsa-form-improper-list-p form) :to-be-truthy)
        (expect form :to-print-as "(a quote . c)")))

    (it "should read an expanded improper list"
      (elsa-test-with-read-form "|(a . (b . (c . d)))" form
        (expect (elsa-form-improper-list-p form) :to-be-truthy)
        (expect form :to-print-as "(a b c . d)")))

    (it "should read an improper list containing a cons pair"
      (elsa-test-with-read-form "|(a . (b . ((c . d) . e)))" form
        (expect (elsa-form-improper-list-p form) :to-be-truthy)
        (expect form :to-print-as "(a b (c . d) . e)"))))

  (describe "lists"

    (it "should read a list written in dot notation"
      (elsa-test-with-read-form "(a . (b))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a b)")))

    (it "should read a list with a list cdr in dot notation"
      (elsa-test-with-read-form "(a . ((b)))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a (b))")))

    (it "should read a list with a cons pair in the middle"
      (elsa-test-with-read-form "(a . ((b . c) d))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a (b . c) d)")))

    (it "should read a list split in a head and a dotted cdr which is a list"
      (elsa-test-with-read-form "(a b . (c d))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a b c d)")))

    (it "should read a list in dot notation with cdr having nested lists"
      (elsa-test-with-read-form "(a . ((b) (x)))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a (b) (x))")))

    (it "should read a short proper list in dotted notation"
      (elsa-test-with-read-form "(a . (b . nil))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a b)")))

    (it "should read a list with the cdr having a nested list in dot notation"
      (elsa-test-with-read-form "(a . ((b . nil)))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a (b))")))

    (it "should read a nested list with the cdr having a nested list in dot notation"
      (elsa-test-with-read-form "((((a . (b . nil)))))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "((((a b))))")))

    (it "should read a long list in dot notation with a nested dotted list"
      (elsa-test-with-read-form "(a . (b . ((c . nil) x)))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a b (c) x)")))

    (it "should read a regular list"
      (elsa-test-with-read-form "(a b c)" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a b c)")))

    (it "should read a nested regular list"
      (elsa-test-with-read-form "((a b c))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "((a b c))")))

    (it "should read a deeply nested regular list"
      (elsa-test-with-read-form "(((a b c)))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(((a b c)))")))

    (it "should read a long proper list in dotted notation"
      (elsa-test-with-read-form "(a . (b . (c . (d . nil))))" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(a b c d)")))

    (describe "with quoted cdr"


      (it "should read a dotted list with a quoted symbol in cdr"
        (elsa-test-with-read-form "|(foo . 'bar)" form
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-print form) :to-equal "(foo quote bar)")))

      (it "should read a dotted list with an expanded quoted symbol in cdr"
        (elsa-test-with-read-form "|(foo . (quote bar))" form
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-print form) :to-equal "(foo quote bar)")))

      (it "should read a dotted list with a quoted symbol in nested cdr"
        (elsa-test-with-read-form "|(foo . (quote . 'bar))" form
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-print form) :to-equal "(foo quote quote bar)")))

      (it "should read a backquoted list with an unquoted symbol in cdr"
        (elsa-test-with-read-form "|`(foo . ,bar)" form
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-print form) :to-equal "(` (foo , bar))")))

      (it "should read a list with an unquoted symbol in cdr"
        (elsa-test-with-read-form "|(foo . ,bar)" form
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-print form) :to-equal "(foo , bar)")))

      (it "should read a list in dotted notation with expanded quoted last cdr"
        (elsa-test-with-read-form "(foo . (quote . (quote (bar baz))))" form
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-print form) :to-equal "(foo quote quote (bar baz))")))

      (it "should read a list in dotted notation with quoted last cdr"
        (elsa-test-with-read-form "(foo . (quote . '(bar baz)))" form
          (expect (elsa-form-list-p form) :to-be-truthy)
          (expect (elsa-form-print form) :to-equal "(foo quote quote (bar baz))"))))

    (describe "setting parents"

      (it "should set parent of forms in a list to the list form"
        (elsa-test-with-read-form "(a b c)" form
          (expect (oref (elsa-nth 0 form) parent) :to-be form)
          (expect (oref (elsa-nth 1 form) parent) :to-be form)
          (expect (oref (elsa-nth 2 form) parent) :to-be form))))

    (describe "setting previous"

      (it "should set previous of forms in a list to the previous form"
        (elsa-test-with-read-form "(a b c)" form
          (expect (oref (elsa-nth 0 form) previous) :to-be nil)
          (expect (oref (elsa-nth 1 form) previous) :to-be (elsa-nth 0 form))
          (expect (oref (elsa-nth 2 form) previous) :to-be (elsa-nth 1 form))))))

  (describe "functions"

    (it "should read byte-compiled function"
      (elsa-test-with-read-form "#[(a) \"\\300\\207\" [nil] 1]" form
        (expect (elsa-form-function-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "#[(a) \"\\300\\207\" [nil] 1]"))))

  (describe "quotes"

    (it "should read a quoted symbol"
      (elsa-test-with-read-form "|'foo" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-get-name form) :to-be 'quote)
        (expect (oref form quote-type) :to-be 'quote)))

    (it "should read an expanded quoted symbol"
      (elsa-test-with-read-form "|(quote foo)" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-get-name form) :to-be 'quote)
        (expect (oref form quote-type) :to-be 'quote)))

    (it "should read an expanded quoted symbol nested inside a list"
      (elsa-test-with-read-form "|(foo (quote foo) bar)" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(foo (quote foo) bar)")))

    (it "should read a quoted function"
      (elsa-test-with-read-form "|#'foo" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-get-name form) :to-be 'function)
        (expect (oref form quote-type) :to-be 'function)))

    (it "should read an expanded quoted function"
      (elsa-test-with-read-form "|(function foo)" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-get-name form) :to-be 'function)
        (expect (oref form quote-type) :to-be 'function)))

    (it "should set line and column of a quote symbol"
      (elsa-test-with-read-form "|(function foo)" form
        (expect (oref (elsa-car form) line) :to-be 1)))

    (it "should set line and column of a quote character"
      (elsa-test-with-read-form "|'foo" form
        (expect (oref (elsa-car form) line) :to-be 1)))

    (it "should read an expanded quoted function nested inside a list"
      (elsa-test-with-read-form "|(foo (function foo) bar)" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-print form) :to-equal "(foo (function foo) bar)")))

    (it "should read an unquoted symbol"
      (elsa-test-with-read-form "|,foo" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-get-name form) :to-be '\,)
        (expect (oref form quote-type) :to-be '\,)))

    (it "should read a backquoted symbol"
      (elsa-test-with-read-form "|`foo" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-get-name form) :to-be '\`)
        (expect (oref form quote-type) :to-be '\`)))

    (it "should read a spliced symbol"
      (elsa-test-with-read-form "|,@foo" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-get-name form) :to-be '\,@)
        (expect (oref form quote-type) :to-be '\,@)))

    (it "should read a list containing quoted nil"
      (elsa-test-with-read-form "|(foo 'nil)" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-list-p (cadr (oref form sequence))) :to-be-truthy)))

    (it "should read a symbol prefixed with a quote followed by unquote"
      (elsa-test-with-read-form "',foo" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-list-p (elsa-cadr form)) :to-be-truthy)
        (expect (oref (elsa-cadr form) quote-type) :to-be '\,)))

    (it "should read a symbol prefixed with a quote followed by splice"
      (elsa-test-with-read-form "',@foo" form
        (expect (elsa-form-list-p form) :to-be-truthy)
        (expect (elsa-form-list-p (elsa-cadr form)) :to-be-truthy)
        (expect (oref (elsa-cadr form) quote-type) :to-be '\,@)))

    (it "should read a quoted quote without expecting that a quote has one `cadr' param"
      (elsa-test-with-read-form "(quote (function . foo))" form
        (expect (elsa-form-list-p form) :to-be-truthy)))))
