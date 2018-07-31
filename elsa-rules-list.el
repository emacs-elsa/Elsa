(require 'elsa-error)
(require 'elsa-check)
(require 'elsa-scope)

(defclass elsa-check-if (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-if) form scope)
  (elsa-form-function-call-p form 'if))

(defclass elsa-check-if-useless-condition (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-useless-condition) form scope)
  (let ((condition (cadr (oref form sequence)))
        (errors))
    (if (not (elsa-type-accept (oref condition type) (elsa-type-nil)))
        (push (elsa-make-warning "Condition always evaluates to true." condition)
              errors)
      (when (elsa-type-accept (elsa-type-nil) (oref condition type))
        (push (elsa-make-warning "Condition always evaluates to false." condition)
              errors)))
    errors))

(add-to-list 'elsa-checks (elsa-check-if-useless-condition))

(defclass elsa-check-if-useless-then-progn (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-useless-then-progn) form scope)
  (let ((then-body (nth 2 (oref form sequence))))
    (when (and (eq (elsa-form-name then-body) 'progn)
               (= 2 (length (oref then-body sequence))))
      (elsa-warning
       :expression then-body
       :message "Useless `progn' around body of then branch."))))

(add-to-list 'elsa-checks (elsa-check-if-useless-then-progn))

(defclass elsa-check-if-useless-else-progn (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-useless-else-progn) form scope)
  (let ((else-body (nth 3 (oref form sequence))))
    (when (eq (elsa-form-name else-body) 'progn)
      (elsa-warning
       :expression else-body
       :message "Useless `progn' around body of else branch."))))

(add-to-list 'elsa-checks (elsa-check-if-useless-else-progn))

(defclass elsa-check-if-to-when (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-to-when) form scope)
  (let ((then-body (nth 2 (oref form sequence)))
        (else-body (nth 3 (oref form sequence))))
    (unless else-body
      (when (eq (elsa-form-name then-body) 'progn)
        (elsa-warning
         :expression then-body
         :message "Use `when' instead of `if' with the then branch wrapped in `progn'")))))

(add-to-list 'elsa-checks (elsa-check-if-to-when))

(defclass elsa-check-symbol (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-symbol) form scope)
  (elsa-form-symbol-p form))

(defclass elsa-check-symbol-naming (elsa-check-symbol) ())

(cl-defmethod elsa-check-check ((_ elsa-check-symbol-naming) form scope)
  (let ((name (symbol-name (elsa-form-name form)))
        (errors))
    (when (string-match-p ".+_" name)
      (push (elsa-warning
             :expression form
             :message "Use lisp-case for naming symbol instead of snake_case.")
            errors))
    (let ((case-fold-search nil))
      (when (string-match-p ".+[a-z][A-Z]" name)
        (push (elsa-warning
               :expression form
               :message "Use lisp-case for naming symbol instead of camelCase.")
              errors)))
    errors))

(add-to-list 'elsa-checks (elsa-check-symbol-naming))

(defclass elsa-check-error-message (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-error-message) form scope)
  (elsa-form-function-call-p form 'error))

(cl-defmethod elsa-check-check ((_ elsa-check-error-message) form scope)
  (let ((error-message (nth 1 (oref form sequence)))
        (errors))
    (when (elsa-form-string-p error-message)
      (let ((msg (oref error-message sequence)))
        (when (equal (substring msg -1) ".")
          (push (elsa-warning
                 :expression error-message
                 :message "Error messages should not end with a period.")
                errors))
        (let ((case-fold-search nil))
          (unless (string-match-p "[A-Z]" msg)
            (push (elsa-warning
                   :expression error-message
                   :message "Error messages should start with a capital letter.")
                  errors)))))
    errors))

(add-to-list 'elsa-checks (elsa-check-error-message))

(provide 'elsa-rules-list)
