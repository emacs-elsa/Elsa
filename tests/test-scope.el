;; -*- lexical-binding: t -*-

(require 'elsa-test-helpers)
(require 'elsa-scope)

(defmacro elsa-test-with-scope (scope vars &rest body)
  (declare (indent 2))
  (let ((hashtable (make-symbol "hashtable")))
    `(let ((,hashtable (make-hash-table)))
       (-each ',vars
         (-lambda ((name . type))
           (puthash name (elsa-variable
                          :name name
                          :type (eval `(elsa-make-type ,@type))) ,hashtable)))
       (let ((,scope (elsa-scope :vars ,hashtable)))
         ,@body))))

(describe "Scope"

  (describe "retrieving variables"

    (it "should retrieve variable by name"
      (let ((scope (elsa-scope)))
        (elsa-scope-add-var scope (elsa-make-variable 'a (elsa-make-type Int)))
        (elsa-scope-add-var scope (elsa-make-variable 'a (elsa-make-type String)))
        (expect (elsa-scope-get-var scope 'a) :to-be-type-equivalent
                (elsa-make-type String))))

    (it "should retrieve variable by name if the scope was protected"
      (let ((scope (elsa-scope)))
        (elsa-scope-add-var scope (elsa-make-variable 'a (elsa-make-type Int)))
        (elsa-save-scope scope
          (expect (elsa-scope-get-var scope 'a) :to-be-type-equivalent
                  (elsa-make-type Int)))))

    (it "should retrieve variable by name if it was assigned with variable bound before"
      (let ((scope (elsa-scope)))
        (elsa-scope-add-var scope (elsa-make-variable 'a (elsa-make-type Int)))
        (elsa-scope-assign-var scope (elsa-make-variable 'a (elsa-make-type String)))
        (expect (elsa-scope-get-var scope 'a) :to-be-type-equivalent
                (elsa-make-type String))))

    (it "should retrieve variable by name if it was assigned with no variable bound before"
      (let ((scope (elsa-scope)))
        (elsa-scope-assign-var scope (elsa-make-variable 'a (elsa-make-type Int)))
        (elsa-save-scope scope
          (expect (elsa-scope-get-var scope 'a) :to-be-type-equivalent
                  (elsa-make-type Int))))))

  (describe "saving scope"

    (it "should not have a variable that was assigned during saved scope after it is restored"
      (let ((scope (elsa-scope)))
        (elsa-save-scope scope
          (elsa-scope-assign-var scope
            (elsa-make-variable 'a (elsa-make-type Int))))
        (expect (elsa-scope-get-var scope 'a) :not :to-be-truthy)))

    (it "should restore binding from before the saving"
      (let ((scope (elsa-scope)))
        (elsa-scope-add-var scope (elsa-make-variable 'a (elsa-make-type Int)))
        (elsa-save-scope scope
          (elsa-scope-assign-var scope
            (elsa-make-variable 'a (elsa-make-type String)))
          (expect (elsa-scope-get-var scope 'a) :to-be-type-equivalent
                  (elsa-make-type String)))
        (expect (elsa-scope-get-var scope 'a) :to-be-type-equivalent
                (elsa-make-type Int))))))
