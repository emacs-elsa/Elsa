(require 'elsa-error)
(require 'elsa-check)

(defun elsa-check--if-p (form)
  "Check if FORM is an `if' form."
  (and (elsa-form-list-p form)
       (eq 'if (elsa-form-name form))))

(defclass elsa-check-if (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-if) form)
  (elsa-check--if-p form))

(defclass elsa-check-if-useless-condition (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-useless-condition) form)
  (let ((condition (cadr (oref form sequence)))
        (errors))
    (if (not (elsa-type-accept (oref condition type) (elsa-type-nil)))
        (push (elsa-warning
               :expression condition
               :message "Condition always evaluates to true.")
              errors)
      (when (elsa-type-accept (elsa-type-nil) (oref condition type))
        (push (elsa-warning
               :expression condition
               :message "Condition always evaluates to false.")
              errors)))
    errors))

(add-to-list 'elsa-checks (elsa-check-if-useless-condition))

(defclass elsa-check-if-useless-then-progn (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-useless-then-progn) form)
  (let ((then-body (nth 2 (oref form sequence))))
    (when (and (eq (elsa-form-name then-body) 'progn)
               (= 2 (length (oref then-body sequence))))
      (elsa-warning
       :expression then-body
       :message "Useless `progn' around body of then branch."))))

(add-to-list 'elsa-checks (elsa-check-if-useless-then-progn))

(defclass elsa-check-if-useless-else-progn (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-useless-else-progn) form)
  (let ((else-body (nth 3 (oref form sequence))))
    (when (eq (elsa-form-name else-body) 'progn)
      (elsa-warning
       :expression else-body
       :message "Useless `progn' around body of else branch."))))

(add-to-list 'elsa-checks (elsa-check-if-useless-else-progn))

(defclass elsa-check-if-to-when (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-to-when) form)
  (let ((then-body (nth 2 (oref form sequence)))
        (else-body (nth 3 (oref form sequence))))
    (unless else-body
      (when (eq (elsa-form-name then-body) 'progn)
        (elsa-warning
         :expression then-body
         :message "Use `when' instead of `if' with the then branch wrapped in `progn'")))))

(add-to-list 'elsa-checks (elsa-check-if-to-when))

(provide 'elsa-rules-list)
