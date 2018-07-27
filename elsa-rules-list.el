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

(provide 'elsa-rules-list)
