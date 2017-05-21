;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa-defun)

(describe "Elsa Defun"


  (describe "Return type"

    (it "should parse the declarations for return type"
      (let ((type (elsa-defun--get-return-type
                   '((foo bar) (elsa-return string)))))
        (expect (elsa-type-string-p type) :to-be-truthy)))

    (it "should use mixed type for missing return type"
      (let ((type (elsa-defun--get-return-type '((foo bar)))))
        (expect (elsa-type-accept type (elsa-make-type 'mixed)) :to-be-truthy))))


  (describe "Argument types"

    (it "should use types to annotate arguments"
      (let ((types (elsa-defun--get-typed-args (list 'a 'b) (list 'string nil))))
        (expect (elsa-type-string-p (cdr (assq 'a types))) :to-be-truthy)
        (expect (elsa-type-nil-p (cdr (assq 'b types))) :to-be-truthy)))

    (it "should use as much types as possible and used mixed type for the rest"
      (let ((types (elsa-defun--get-typed-args (list 'a 'b) (list 'string))))
        (expect (elsa-type-string-p (cdr (assq 'a types))) :to-be-truthy)
        (expect (elsa-type-accept (cdr (assq 'b types))
                                  (elsa-make-type 'mixed))
                :to-be-truthy)))

    (it "should use mixed type for untyped arguments"
      (let ((types (elsa-defun--get-typed-args (list 'a 'b) nil)))
        (expect (elsa-type-accept (cdr (assq 'a types))
                                  (elsa-make-type 'mixed))
                :to-be-truthy)
        (expect (elsa-type-accept (cdr (assq 'b types))
                                  (elsa-make-type 'mixed))
                :to-be-truthy)))

    (it "should remember the argument list &-markers"
      (let ((types (elsa-defun--get-typed-args
                    (list 'a '&optional 'b '&rest 'c)
                    (list 'string 'string 'mixed))))
        (expect (elsa-type-string-p (cdr (assq 'a types))) :to-be-truthy)
        (expect (elsa-type-string-p (cdr (assq 'b types))) :to-be-truthy)
        (expect (elsa-type-accept (cdr (assq 'c types))
                                  (elsa-make-type 'mixed))
                :to-be-truthy)
        (expect (nth 1 types) :to-equal (list '&optional))
        (expect (nth 3 types) :to-equal (list '&rest)))))


  (describe "Defun factory"

    (it "should create new defun according to arguments"
      (let ((def (elsa-make-defun 'foo (list 'a)
                                  '((elsa-return string)
                                    (elsa-args string)))))
        (expect (elsa-defun-p def) :to-be-truthy)
        (expect (oref def name) :to-equal 'foo)
        (expect (listp (oref def args)) :to-be-truthy)
        (expect (elsa-type-string-p (oref def return-type)) :to-be-truthy)))))
