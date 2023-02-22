;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa-analyser)

(describe "Elsa analyser - special forms"

  (before-each
    (put 'a 'elsa-type nil))

  (describe "Special forms"

    (describe "defvar"

      (after-each (put 'foo 'elsa-type-var nil))

      (it "should analyze the default form of the defvar and assign that type"
        (elsa-test-with-analysed-form "(progn (defvar foo :keyword) foo)" form
          (expect (elsa-nth 2 form) :to-be-type-equivalent (elsa-make-type (const :keyword)))))

      (it "should respect the type assigned from an annotation"
        (elsa-test-with-analysed-form ";; (foo :: bool)\n(progn (defvar foo :keyword) foo)" form
          (expect (elsa-nth 2 form) :to-be-type-equivalent (elsa-make-type bool)))))

    (describe "defconst"

      (after-each (put 'foo 'elsa-type-var nil))

      (it "should analyze the default form of the defconst and assign that type"
        (elsa-test-with-analysed-form "(progn (defconst foo 'bar) foo)" form
          (let ((actual (elsa-get-type (elsa-nth 2 form))))
            (expect (elsa-readonly-type-p actual))
            (expect (oref actual type) :to-be-type-equivalent (elsa-make-type (const bar))))))

      (it "should respect the type assigned from an annotation"
        (elsa-test-with-analysed-form ";; (foo :: bool)\n(progn (defconst foo :keyword) foo)" form
          (expect (elsa-type-describe (elsa-get-type (elsa-nth 2 form))) :to-equal
                  "(readonly bool)"))))

    (describe "quote"

      (it "should resolve to type list if the quoted argument is a list"
        (elsa-test-with-analysed-form "'(foo)" form
          (expect (elsa-type-list-p (oref form type)) :to-be-truthy))))

    (describe "and"

      (describe "return type analysis"

        (it "should set return type to nil if we empty domain of some variable"
          (elsa-test-with-analysed-form "|(defun a (x) (and (stringp x) (integerp x)))" form
            (let ((and-form (elsa-nth 3 form)))
              (expect and-form :to-be-type-equivalent (elsa-type-nil)))))

        (it "should set return type to nil if there is a surely nil expression"
          (elsa-test-with-analysed-form "|(defun a (x) (and nil (1+ x)))" form
            (let ((and-form (elsa-nth 3 form)))
              (expect and-form :to-be-type-equivalent (elsa-type-nil)))))

        (it "should set return type to narrowed variable type"
          (elsa-test-with-analysed-form "|(defun a (x) (and (stringp x) x))" form
            (let ((and-form (elsa-nth 3 form)))
              (expect and-form :to-be-type-equivalent (elsa-make-type (or string nil))))))

        (it "should have nullable return type if the last condition might be reached and is possibly nil"
          (elsa-test-with-analysed-form "|(defun a (x) (let ((y 1)) (and x y)))" form
            (let ((and-form (elsa-nth 3 form)))
              (expect and-form :to-be-type-equivalent (elsa-make-type (or nil (const 1)))))))

        (it "should set return type to t for empty (and) form"
          (elsa-test-with-analysed-form "|(and)" form
            (expect form :to-be-type-equivalent (elsa-type-t)))))

      (describe "narrowing types"

        (it "should narrow the types in then-branch by all the expressions because they must all be true to enter"
          (elsa-test-with-analysed-form "|(defun a (x y) (if (and (integerp x) (integerp y)) (progn x y) (progn x y)))" form
            (let* ((progn-form (elsa-nth 2 (elsa-nth 3 form)))
                   (x-form (elsa-nth 1 progn-form))
                   (y-form (elsa-nth 2 progn-form)))
              (expect x-form :to-be-type-equivalent (elsa-make-type int))
              (expect y-form :to-be-type-equivalent (elsa-make-type int)))))

        (it "should not narrow the types in else-branch by the reachable expressions where we don't know which may fail"
          ;;  (integerp x) | (integerp y) | p |
          ;; --------------+--------------+---+------------
          ;;       t       |      t       |   |  possible    tt does not propagate
          ;; --------------+--------------+---+------------
          ;;       t       |     nil      | p |  possible
          ;;      nil      |      t       | p |  possible
          ;;      nil      |     nil      | p |  possible
          (elsa-test-with-analysed-form "|(defun a (x y) (if (and (integerp x) (integerp y)) (progn x y) (progn x y)))" form
            (let* ((progn-form (elsa-nth 3 (elsa-nth 3 form)))
                   (x-form (elsa-nth 1 progn-form))
                   (y-form (elsa-nth 2 progn-form)))
              (expect x-form :to-be-type-equivalent (elsa-make-type mixed))
              (expect y-form :to-be-type-equivalent (elsa-make-type mixed)))))

        (it "should empty the domain in case the tests are incompatible"
          (elsa-test-with-analysed-form "|(defun a (x) (and (stringp x) (integerp x) x))" form
            (let ((test-form (elsa-nth 3 (elsa-nth 3 form))))
              (expect test-form :to-be-type-equivalent (elsa-type-empty)))))

        (describe "propagation of narrowing to \"else\" branch"

          (it "should propagate diff-narrowing if the narrowing form forces following forms to be necessarily true"
            ;; Here the (symbolp x) is unnecessary, because at that
            ;; point the domain of x is keyword.
            ;;  (keywordp x) | (symbolp x) | p |            |
            ;; --------------+-------------+---+------------+
            ;;       t       |      t      |   | necessary  | tt does not propagate
            ;; --------------+-------------+---+------------+
            ;;       t       |     nil     |   | impossible |
            ;; --------------+-------------+---+------------+
            ;;      nil      |      t      | p |  possible  | keywordp always nil
            ;;      nil      |     nil     | p |  possible  | keywordp always nil
            (elsa-test-with-analysed-form "|(defun a (x) (or (and (keywordp x) (symbolp x)) x))" form
              (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
                (expect (elsa-get-type test-form) :to-be-type-equivalent
                        (elsa-type-diff (elsa-type-mixed) (elsa-type-keyword))))))

          (it "should propagate diff-narrowing if the narrowing form is conditionally deciding based on the previous forms"
            ;; Here, assuming (symbolp x) succeded, keywordp becomes a
            ;; deciding form.  It can never succeed if (symbolp x)
            ;; failed.
            ;;  (symbolp x) | (keywordp x) | p |
            ;; -------------+--------------+---+------------
            ;;       t      |      t       |   |  possible    tt does not propagate
            ;;       t      |     nil      | p |  possible    keywordp always nil
            ;; -------------+--------------+---+------------
            ;;      nil     |     nil      | p | necessary    keywordp always nil
            ;; -------------+--------------+---+------------
            ;;      nil     |      t       |   | impossible
            (elsa-test-with-analysed-form "|(defun a (x) (or (and (symbolp x) (keywordp x)) x))" form
              (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
                (expect (elsa-get-type test-form) :to-be-type-equivalent
                        (elsa-type-diff (elsa-type-mixed) (elsa-type-keyword))))))

          (it "should propagate diff-narrowing if the narrowing form is necessarily true if it succeeds"
            ;; concat always returns non-nil if x is string
            ;;  (stringp x) | (concat "" x) | p |
            ;; -------------+---------------+---+------------
            ;;      t       |       t       |   | necessary    tt does not propagate
            ;;     nil      |      nil      | p | necessary    stringp always nil
            ;; -------------+---------------+---+------------
            ;;      t       |      nil      |   | impossible
            ;;     nil      |       t       |   | impossible
            (elsa-test-with-analysed-form "|(defun a (x) (or (and (stringp x) (concat \"\" x)) x))" form
              (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
                (expect (elsa-get-type test-form) :to-be-type-equivalent
                        (elsa-type-diff (elsa-type-mixed) (elsa-type-string))))))

          (it "should not propagate diff-narrowing if the narrowed variable's domain empties"
            ;; Here x can't be string and integer at the same time, so
            ;; either form can cause the and to fail.
            ;;  (stringp x) | (integerp x) | p |
            ;; -------------+--------------+---+------------
            ;;      t       |      t       |   | impossible    tt does not propagate
            ;; -------------+--------------+---+------------
            ;;      t       |     nil      | p | necessary
            ;; -------------+--------------+---+------------
            ;;     nil      |      t       | p |  possible
            ;;     nil      |     nil      | p |  possible
            (elsa-test-with-analysed-form "|(defun a (x) (or (and (stringp x) (integerp x)) x))" form
              (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
                (expect (elsa-get-type test-form) :to-be-type-equivalent
                        (elsa-type-mixed)))))

          (it "should not propagate diff-narrowing if the narrowing form can fail"
            ;; Irrespective of whether x is string or not, the second
            ;; form can decide the result of the entire and form.
            ;;  (stringp x) | (mixed) | p |
            ;; -------------+---------+---+----------
            ;;      t       |    t    |   | possible   tt does not propagate
            ;; -------------+---------+---+----------
            ;;     nil      |    t    | p | possible
            ;;      t       |   nil   | p | possible   x can be anything in else
            ;;     nil      |   nil   | p | possible
            ;; -------------+---------+---+----------
            (elsa-test-with-analysed-form "|(defun a (x) (or (and (stringp x) (mixed-function)) x))" form
              (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
                (message "type %s" (elsa-tostring (elsa-get-type test-form)))
                (expect (elsa-get-type test-form) :to-be-type-equivalent
                        (elsa-type-mixed))))))

        (it "should return nil if one condition rules out the other"
          (elsa-test-with-analysed-form "|(defun a (x y) (and x (unless x y)))" form
            (let ((and-form (elsa-nth 3 form)))
              (expect and-form :to-be-type-equivalent (elsa-make-type nil)))))

        (it "should restore the type after the form"
          (elsa-test-with-analysed-form "|(defun a (x) (and (stringp x) (integerp x) x) x)" form
            (let ((test-form (elsa-nth 4 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type mixed)))))

        (it "should respect setq overriding the narrowed type inside the form"
          (elsa-test-with-analysed-form "|(defun a (x) (and (stringp x) (setq x :key) x) x)" form
            (let ((test-form (elsa-nth 3 (elsa-nth 3 form))))
              (expect test-form :to-be-type-equivalent (elsa-make-type (const :key)))))))

      (xdescribe "setq propagation"

        (it "should use setq type outside of the and form if the expression was surely executed"
          (elsa-test-with-analysed-form "|(defun a (x) (and (setq x :key)) x)" form
            (let ((test-form (elsa-nth 4 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (const :key))))))

        (it "should use setq type outside of the and form if the expression was surely executed even if the and condition failed"
          (elsa-test-with-analysed-form "|(defun a (x) (and (setq x :key) nil) x)" form
            (let ((test-form (elsa-nth 4 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (const :key))))))

        (it "should not use setq type outside of the and form if the expression was never executed"
          (elsa-test-with-analysed-form "|(defun a (x) (and nil (setq x :key)) x)" form
            (let ((test-form (elsa-nth 4 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type mixed)))))

        (it "should sum setq type and scope type outside of the and form if the expression was maybe executed"
          (elsa-test-with-analysed-form "|(let ((x 'foo)) (and (foo x) (setq x :key)) x)" form
            (let ((test-form (elsa-nth 4 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type symbol)))))))

    (describe "or"

      (describe "return type analysis"

        (it "should set return type to nullable sum if there is no sure single branch."
          (elsa-test-with-analysed-form "|(defun a (x) (or (and (stringp x) x)))" form
            (let ((or-form (elsa-nth 3 form)))
              (expect or-form :to-be-type-equivalent
                      (elsa-make-type (or string nil))))))

        (it "should set return type to sum of narrowed types"
          (elsa-test-with-analysed-form "|(defun a (x) (or (stringp x) x))" form
            (let ((or-form (elsa-nth 3 form)))
              (expect or-form :to-be-type-equivalent
                      (elsa-type-sum
                       (elsa-type-t)
                       (elsa-type-diff
                        (elsa-type-mixed)
                        (elsa-type-string)))))))

        (it "should set return type to nullable sum if there is no sure branch of multiple branches."
          (elsa-test-with-analysed-form "|(defun a (x) (or (and (stringp x) x) (and (integerp x) x)))" form
            (let ((or-form (elsa-nth 3 form)))
              (expect (elsa-type-describe
                       (elsa-get-type or-form))
                      :to-equal "(or string nil int)"))))

        (it "should set return type to non-nullable sum if there is a sure branch."
          (elsa-test-with-analysed-form "|(defun a (x) (or (stringp x) 2 nil))" form
            (let ((or-form (elsa-nth 3 form)))
              (expect or-form :to-be-type-equivalent
                      (elsa-make-type (or t (const 2)))))))

        (it "should set return type to nil for empty (or) form"
          (elsa-test-with-analysed-form "|(or)" form
            (expect form :to-be-type-equivalent (elsa-type-nil))))

        (it "should set return type to sum of return types for function overload"
          (elsa-test-with-analysed-form "|(downcase x)" form
            (expect form :to-be-type-equivalent (elsa-make-type (or string int))))))

      (describe "narrowing types"

        (it "should narrow the type in the subsequent expressions of or form"
          (elsa-test-with-analysed-form "|(defun a (x) (or (stringp x) x))" form
            (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
              (expect test-form :to-be-type-equivalent (elsa-type-diff (elsa-type-mixed) (elsa-type-string))))))

        (it "should not narrow the type by the unreachable expressions"
          (elsa-test-with-analysed-form "|(defun a (x) (if (or t x) x x))" form
            (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
              (message "%s" (elsa-tostring (elsa-get-type test-form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type mixed)))))

        (it "should sum the complements of narrowed types in subsequent conditions"
          (elsa-test-with-analysed-form "|(defun a (x) (or (stringp x) (integerp x) x))" form
            (let ((test-form (elsa-nth 3 (elsa-nth 3 form))))
              (expect test-form :to-be-type-equivalent
                      (elsa-type-diff (elsa-type-mixed)
                                      (elsa-make-type (or string int)))))))

        (it "should restore the type after the form"
          (elsa-test-with-analysed-form "|(defun a (x) (or (stringp x) (integerp x) x) x)" form
            (let ((test-form (elsa-nth 4 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type mixed)))))

        (it "should respect setq adding the type to the narrowing sum in the then-body"
          (elsa-test-with-analysed-form "|(defun a (x) (if (or (stringp x) (setq x :key) x) x x) x)" form
            (let ((test-form (elsa-nth 2 (elsa-nth 3 form))))
              (expect test-form :to-be-type-equivalent
                      (elsa-type-sum (elsa-make-type string) (elsa-make-type (const :key)))))))

        (it "should empty the domain after setq with assignment of non-nil value"
          (elsa-test-with-analysed-form "|(defun a (x) (or (stringp x) (setq x :key) x) x)" form
            (let ((test-form (elsa-nth 3 (elsa-nth 3 form))))
              (expect test-form :to-be-type-equivalent (elsa-type-empty))))))

      (xdescribe "setq propagation"

        (it "should use setq type outside of the or form if the expression was surely executed"
          (elsa-test-with-analysed-form "|(defun a (x) (or (setq x :key)) x)" form
            (let ((test-form (elsa-nth 4 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (const :key))))))

        (it "should not use setq type outside of the or form if the expression was never executed"
          (elsa-test-with-analysed-form "|(defun a (x) (or t (setq x :key)) x)" form
            (let ((test-form (elsa-nth 4 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type mixed)))))

        (it "should sum setq type and scope type outside of the or form if the expression was maybe executed"
          (elsa-test-with-analysed-form "|(let ((x 'foo)) (or (foo x) (setq x :key)) x)" form
            (let ((test-form (elsa-nth 4 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (or symbol (const :key)))))))))

    (describe "if"

      (describe "return type analysis"

        (it "should return true-body type if condition is always true and false body is missing"
          (elsa-test-with-analysed-form "|(if t 1)" form
            (expect form :to-be-type-equivalent (elsa-make-type (const 1)))))

        (it "should return nil if condition is always false and false body is missing"
          (elsa-test-with-analysed-form "|(if nil 1)" form
            (expect form :to-be-type-equivalent (elsa-type-nil))))

        (it "should return sum of true-body and false-body if condition is neither always true nor false."
          (elsa-test-with-analysed-form "|(defun a (x) (if x 1 :key))" form
            (let ((test-form (elsa-nth 3 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (or (const 1) (const :key)))))))

        (it "should return true-body type if condition is always true and false body is defined"
          (elsa-test-with-analysed-form "|(if t 1 :key)" form
            (expect form :to-be-type-equivalent (elsa-make-type (const 1)))))

        (it "should return false-body type if condition is always false and false body is defined"
          (elsa-test-with-analysed-form "|(if nil 1 :key)" form
            (expect form :to-be-type-equivalent (elsa-make-type (const :key))))))

      (describe "narrowing types"

        (it "should narrow a variable to its type in true body"
          (elsa-test-with-analysed-form "|(defun a (x) (if x x))" form
            (let ((second-cond (elsa-nth 2 (elsa-nth 3 form))))
              (expect (oref second-cond type) :to-be-type-equivalent
                      (elsa-type-diff (elsa-type-mixed) (elsa-type-nil))))))

        (it "should narrow a variable to nil in else body"
          (elsa-test-with-analysed-form "|(defun a (x) (if x x x))" form
            (let ((second-cond (elsa-nth 3 (elsa-nth 3 form))))
              (expect (oref second-cond type) :to-be-type-equivalent
                      (elsa-type-nil)))))

        (it "should restore the variable type after the if body"
          (elsa-test-with-analysed-form "|(defun a (x) (if x x x) x)" form
            (let ((var-form (elsa-nth 4 form)))
              (expect (oref var-form type) :to-be-type-equivalent
                      (elsa-type-mixed))))))

      (describe "setq propagation"

        (it "should use setq type outside of the if form if used in the conditional"
          (elsa-test-with-analysed-form "|(let (x) (if (setq x :key) 1 2) x)" form
            (let ((test-form (elsa-nth 3 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (const :key))))))

        (it "should not pollute scope outside of the narrowed body"
          (elsa-test-with-analysed-form "|(if x (setq x :key) x)" form
            (let ((test-form (elsa-nth 3 form)))
              (expect (elsa-tostring (elsa-type-normalize (elsa-get-type test-form)))
                      :to-equal
                      (elsa-tostring (elsa-type-nil))))))

        (it "should sum the type from possibly executed true-branch to the parent scope"
          (elsa-test-with-analysed-form "|(let ((a 1)) (if x (setq a :key) x) a)" form
            (let ((test-form (elsa-nth 3 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (or (const 1) (const :key)))))))

        (it "should sum the type from possibly executed false-branch to the parent scope"
          (elsa-test-with-analysed-form "|(let ((a 1)) (if x x (setq a :key)) a)" form
            (let ((test-form (elsa-nth 3 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (or (const 1) (const :key)))))))

        (it "should replace the parent scope type if assignment is present in both branches and sure to execute"
          (elsa-test-with-analysed-form "|(let ((a 1)) (if x (setq a :key) (setq a 1.0)) a)" form
            (let ((test-form (elsa-nth 3 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (or (const 1.0) (const :key)))))))

        (it "should introduce new variables to scope"
          (elsa-test-with-analysed-form "|(progn (if x (setq a :key) (setq a 1)) a)" form
            (let ((test-form (elsa-nth 2 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (or (const 1) (const :key)))))))

        (xit "should not introduce possibly unbound new variables to scope"
          (elsa-test-with-analysed-form "|(progn (if x (setq a :key) 1) a)" form
            (let ((test-form (elsa-nth 2 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type unbound)))))))

    (describe "cond"

      (describe "return type analysis"

        (it "should return nil if cond is empty"
          (elsa-test-with-analysed-form "|(cond)" form
            (expect form :to-be-type-equivalent (elsa-type-nil))))

        (it "should return nil if cond has one empty branch"
          (elsa-test-with-analysed-form "|(cond ())" form
            (expect form :to-be-type-equivalent (elsa-type-nil))))

        (it "should return nil if cond has one empty branch with empty condition"
          (elsa-test-with-analysed-form "|(cond (()))" form
            (expect form :to-be-type-equivalent (elsa-type-nil))))

        (it "should short-circuit if some form's condition is always true"
          (elsa-test-with-analysed-form "|(defun a (x) (cond (t :foo) (x 'bar)))" form
            (let ((test-form (elsa-nth 3 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (const :foo))))))

        (it "should return non-nullable if last condition is catch-all and returns non-nullable"
          (elsa-test-with-analysed-form "|(defun a (x) (cond (x 1) (t :foo)))" form
            (let ((test-form (elsa-nth 3 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (or (const :foo) (const 1)))))))

        (it "should return nullable if none of the conditions is surely true"
          (elsa-test-with-analysed-form "|(defun a (x) (cond (x 1) (x :foo)))" form
            (let ((test-form (elsa-nth 3 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (or (const :foo) (const 1) nil))))))

        (it "should return nullable if last condition is catch-all but returns nullable"
          (elsa-test-with-analysed-form "|(defun a (x y) (cond (x 1) (t (if y :foo nil))))" form
            (let ((test-form (elsa-nth 3 form)))
              (expect test-form :to-be-type-equivalent (elsa-make-type (or (const :foo) (const 1) nil)))))))

      (describe "narrowing types"

        (it "should narrow a variable to its type in the first body"
          (elsa-test-with-analysed-form "|(defun a (x) (cond ((stringp x) x) ((integerp x) x) (t x)))" form
            (let ((test-form (elsa-nth 1 (elsa-nth 1 (elsa-nth 3 form)))))
              (expect test-form :to-be-type-equivalent (elsa-type-string)))))

        (it "should narrow a variable to its type in the second body"
          (elsa-test-with-analysed-form "|(defun a (x) (cond ((stringp x) x) ((integerp x) x) (t x)))" form
            (let ((test-form (elsa-nth 1 (elsa-nth 2 (elsa-nth 3 form)))))
              (expect test-form :to-be-type-equivalent (elsa-type-int)))))

        (it "should use the inference from first body in the second condition"
          (elsa-test-with-analysed-form "|(defun a (x) (cond ((stringp x) x) ((stringp x) x) (t x)))" form
            (let ((test-form (elsa-car (elsa-nth 2 (elsa-nth 3 form)))))
              (expect test-form :to-be-type-equivalent (elsa-type-nil)))))

        (it "should narrow a variable to its type in the last body"
          (elsa-test-with-analysed-form "|(defun a (x) (cond ((stringp x) x) ((integerp x) x) (t x)))" form
            (let ((test-form (elsa-nth 1 (elsa-nth 3 (elsa-nth 3 form)))))
              (expect test-form :to-be-type-equivalent
                      (elsa-type-diff
                       (elsa-type-mixed)
                       (elsa-make-type (or string int)))))))))

    (describe "setq"

      (describe "assignment in condition form"

        (it "should narrow type of place to the type of assignment in true-branch"
          (elsa-test-with-analysed-form "|(if (setq a :key) a a)" form
            (let ((test-form (elsa-nth 2 form)))
              (expect (elsa-get-type test-form) :to-be-type-equivalent (elsa-make-type (const :key))))))

        (it "should narrow type of place to nil in the else-branch"
          (elsa-test-with-analysed-form "|(if (setq a (mixed)) a a)" form
            (let ((test-form (elsa-nth 3 form)))
              (expect (elsa-get-type test-form) :to-be-type-equivalent (elsa-type-nil)))))

        (it "should empty the domain in the else-branch if assignment was always non-nil"
          (elsa-test-with-analysed-form "|(if (setq a :key) a a)" form
            (let ((test-form (elsa-nth 3 form)))
              (expect (elsa-get-type test-form) :to-be-type-equivalent (elsa-type-empty))))))

      (it "should update current scope"
        (elsa-test-with-analysed-form "|(let ((a 1)) (setq a :key) a)" form
          (let ((test-form (elsa-nth 3 form)))
            (expect test-form :to-be-type-equivalent (elsa-make-type (const :key))))))

      (it "should introduce new variables to scope"
        (elsa-test-with-analysed-form "|(progn (setq a :key) a)" form
          (let ((test-form (elsa-nth 2 form)))
            (expect test-form :to-be-type-equivalent (elsa-make-type (const :key))))))

      (it "should not pollute scope outside of the binding form"
        (elsa-test-with-analysed-form "|(progn (let ((a 1)) (setq a :key)) a)" form
          (let ((test-form (elsa-nth 2 form)))
            (expect test-form :to-be-type-equivalent (elsa-type-unbound)))))

      (it "should unassign on top of narrowed variable bindings"
        (elsa-test-with-analysed-form "|(defun a (x) (cond ((stringp x) x) ((integerp x) (setq x :key)) (x)) x)" form
          (let ((test-form (elsa-nth 4 form)))
            (expect test-form :to-be-type-equivalent (elsa-type-mixed)))))

      (after-each (put 'foo 'elsa-type-var nil))

      (it "should warn on assignment to defconst"
        (elsa-test-with-analysed-form "|(progn (defconst foo 'bar) (setq foo 'bar))" form
          :state-var state
          (expect (oref state errors) :not :to-be nil)
          (expect (oref (car (oref state errors)) message) :to-equal
                  "Assignment to read-only variable foo")))

      (it "should allow assignment of read-only type"
        (elsa-test-with-analysed-form "(progn (defconst foo 1)\n\n;; (bar :: int)\n(defvar bar)\n(setq bar foo))" form
          :state-var state
          (expect (oref state errors) :to-be nil)))))

  (describe "Normalize spec"

    (it "should evaluate all arguments when the spec is t"
      (elsa-test-with-analysed-form "|(fun a b c d)" form
        (expect (elsa--analyse-normalize-spec t form) :to-equal (list t t t t))))

    (it "should evaluate all remaining arguments when the spec ends in 'body"
      (elsa-test-with-analysed-form "|(fun a b c d)" form
        (expect (elsa--analyse-normalize-spec (list nil 'body) form) :to-equal (list nil t t t))))

    (it "should keep the spec as provided otherwise"
      (elsa-test-with-analysed-form "|(fun a b c d)" form
        (expect (elsa--analyse-normalize-spec (list nil t nil t) form) :to-equal (list nil t nil t)))))

  (describe "Analyzing symbols"

    (it "should recognize unbound variables."
      (elsa-test-with-analysed-form "|x" form
        (expect form :to-be-type-equivalent (elsa-type-unbound))))

    (it "should recognize lexically bound let-variables"
      (elsa-test-with-analysed-form "|(let ((x 1)) x)" form
        (expect (elsa-nth 2 form) :to-be-type-equivalent (elsa-make-type (const 1)))))

    (it "should not recognize nil as unbound variable"
      (elsa-test-with-analysed-form "|nil" form
        (expect form :to-be-type-equivalent (elsa-type-nil))))

    (it "should not recognize t as unbound variable"
      (elsa-test-with-analysed-form "|t" form
        (expect form :to-be-type-equivalent (elsa-type-t))))

    (it "should not recognize keywords as unbound variable"
      (elsa-test-with-analysed-form "|:foo" form
        (expect form :to-be-type-equivalent (elsa-make-type (const :foo)))))))
