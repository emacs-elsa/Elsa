(require 'elsa-error)
(require 'elsa-check)

(defvar elsa-rules-list nil)

(defun elsa-check--if-p (form)
  "Check if FORM is an `if' form."
  (and (elsa-form-list-p form)
       (eq (oref (elsa-form-car form) name) 'if)))

(defun elsa-check--if-useless-condition (form)
  (let ((condition (cadr (oref form sequence))))
    (when (and (elsa-form-symbol-p condition)
               (eq (oref condition name) t))
      (elsa-warning :message "Condition always evaluates to true"))))

(elsa-check-add
 (elsa-check
  :name 'if-useless-condition
  :predicate 'elsa-check--if-p
  :check 'elsa-check--if-useless-condition))

(provide 'elsa-rules-list)
