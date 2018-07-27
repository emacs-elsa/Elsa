(require 'elsa-error)
(require 'elsa-check)

(defvar elsa-rules-list nil)

(defun elsa-check--if-p (form)
  "Check if FORM is an `if' form."
  (and (elsa-form-list-p form)
       (eq (oref (elsa-form-car form) name) 'if)))

(defun elsa-check--if-useless-condition (form)
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

(elsa-check-add
 (elsa-check
  :name 'if-useless-condition
  :predicate 'elsa-check--if-p
  :check 'elsa-check--if-useless-condition))

(defun elsa-check--if-useless-then-progn (form)
  (let ((then-body (nth 2 (oref form sequence))))
    (when (and (eq (elsa-form-name then-body) 'progn)
               (= 2 (length (oref then-body sequence))))
      (elsa-warning
       :expression then-body
       :message "Useless `progn' around body of then branch."))))

(elsa-check-add
 (elsa-check
  :name 'if-useless-then-progn
  :predicate 'elsa-check--if-p
  :check 'elsa-check--if-useless-then-progn))

(defun elsa-check--if-useless-else-progn (form)
  (let ((else-body (nth 3 (oref form sequence))))
    (when (eq (elsa-form-name else-body) 'progn)
      (elsa-warning
       :expression else-body
       :message "Useless `progn' around body of else branch."))))

(elsa-check-add
 (elsa-check
  :name 'if-useless-else-progn
  :predicate 'elsa-check--if-p
  :check 'elsa-check--if-useless-else-progn))

(defun elsa-check--if-to-when (form)
  (let ((then-body (nth 2 (oref form sequence)))
        (else-body (nth 3 (oref form sequence))))
    (unless else-body
      (when (eq (elsa-form-name then-body) 'progn)
        (elsa-warning
         :expression then-body
         :message "Use `when' instead of `if' with the then branch wrapped in `progn'")))))

(elsa-check-add
 (elsa-check
  :name 'if-to-when
  :predicate 'elsa-check--if-p
  :check 'elsa-check--if-to-when))

(provide 'elsa-rules-list)
