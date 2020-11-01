;; -*- lexical-binding: t -*-

(require 'elsa-undercover)
(require 'elsa)


(xdescribe "let form analysis"


  (it "should update local scope with let-bound variables"
    (let ((state (elsa-state)))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun :args `((x . ,(elsa-make-type int))
                                 (y . ,(elsa-make-type string)))
              :return-type nil))
      (elsa-analyse-let
       state '((a 1) (b "")) '((fun a b)))
      (expect (length (oref state errors)) :to-equal 0)))

  (it "should update local scope in parallel"
    (let ((state (elsa-state)))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun :args `((x . ,(elsa-make-type int))
                                 (y . ,(elsa-make-type string)))
              :return-type nil))
      (elsa-analyse-let
       state '((a 1) (b a)) '((fun a b)))
      (let ((errors (oref state errors)))
        (expect (length errors) :to-equal 2)
        (expect (oref (car errors) message) :to-equal
                "Invalid type, has unbound, expected string")
        (expect (oref (cadr errors) message) :to-equal
                "Unbound variable `a'"))))

  (it "should handle non-list arguments in bindings"
    (let ((state (elsa-state)))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun :args `((x . ,(elsa-make-type int))
                                 (y . ,(elsa-make-type string)))
              :return-type nil))
      (elsa-analyse-let
       state '(a (b nil)) '((fun a b)))
      (let ((errors (oref state errors)))
        (expect (length errors) :to-equal 2)
        (expect (oref (car errors) message) :to-equal
                "Invalid type, has nil, expected string")
        (expect (oref (cadr errors) message) :to-equal
                "Invalid type, has nil, expected int"))))

  ;; TODO: (it "should handle list arguments with no cadr in bindings")

  (it "should restore local scope when exiting nested let forms"
    (let ((state (elsa-state)))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun :args `((x . ,(elsa-make-type int))
                                 (y . ,(elsa-make-type string)))
              :return-type nil))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun2 :args `((x . ,(elsa-make-type string)))
              :return-type nil))
      (elsa-analyse-let
       state '((a 1) (b "")) '((let ((a "")) (fun2 a)) (fun a b)))
      (expect (length (oref state errors)) :to-equal 0))))


(xdescribe "let* form analysis"


  (it "should update local scope with let-bound variables"
    (let ((state (elsa-state)))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun :args `((x . ,(elsa-make-type int))
                                 (y . ,(elsa-make-type string)))
              :return-type nil))
      (elsa-analyse-let*
       state '((a 1) (b "")) '((fun a b)))
      (expect (length (oref state errors)) :to-equal 0)))

  (it "should update local scope in sequence"
    (let ((state (elsa-state)))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun :args `((x . ,(elsa-make-type int))
                                 (y . ,(elsa-make-type int)))
              :return-type nil))
      (elsa-analyse-let*
       state '((a 1) (b a)) '((fun a b)))
      (expect (length (oref state errors)) :to-equal 0)))

  (it "should handle non-list arguments in bindings"
    (let ((state (elsa-state)))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun :args `((x . ,(elsa-make-type int))
                                 (y . ,(elsa-make-type string)))
              :return-type nil))
      (elsa-analyse-let*
       state '(a (b nil)) '((fun a b)))
      (let ((errors (oref state errors)))
        (expect (length errors) :to-equal 2)
        (expect (oref (car errors) message) :to-equal
                "Invalid type, has nil, expected string")
        (expect (oref (cadr errors) message) :to-equal
                "Invalid type, has nil, expected int"))))

  (it "should restore local scope when exiting nested let* forms"
    (let ((state (elsa-state)))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun :args `((x . ,(elsa-make-type int))
                                 (y . ,(elsa-make-type string)))
              :return-type nil))
      (elsa-state-add-defun
       state (elsa-defun
              :name 'fun2 :args `((x . ,(elsa-make-type string)))
              :return-type nil))
      (elsa-analyse-let
       state '((a 1) (b "")) '((let* ((a "")) (fun2 a)) (fun a b)))
      (expect (length (oref state errors)) :to-equal 0))))


(xdescribe "Check defun"
  ;; TODO: testy na &optional atd
  ;; (it "should ignore &optional from function args-types list")
  ;; (it "should add function arguments to local scope")
  )
