(require 'elsa-error)
(require 'elsa-infer)
(require 'elsa-types)

(defvar elsa--infer-if-hook nil)

(defun elsa--infer-if-run-hooks (orig-fn form scope decls)
  (let ((result (funcall orig-fn form scope decls)))
    (run-hook-with-args 'elsa--infer-if-hook result scope decls)
    result))

(advice-add 'elsa--infer-if :around 'elsa--infer-if-run-hooks)

(defun elsa-check-if-unreachable-branch (expr scope decls)
  (let ((condition (oref expr condition)))
    (if (not (elsa-type-accept (oref condition type) (elsa-type-nil)))
        (push (elsa-warning
               :expression condition
               :message "Else clause will never be executed.")
              elsa-infer-errors)
      (when (elsa-type-accept (elsa-type-nil) (oref condition type))
        (push (elsa-warning
               :expression condition
               :message "Then clause will never be executed.")
              elsa-infer-errors)))))

(add-hook 'elsa--infer-if-hook 'elsa-check-if-unreachable-branch)

(defun elsa-check-if-useless-progn (expr scope decls)
  (let* ((form (oref expr form))
         (else (oref form else)))
    (when (eq (eieio-object-class (car else)) 'elsa-form-progn)
      (push (elsa-notice
             :expression (oref expr else)
             :message "Unnecessary `progn' wrapping else branch.")
            elsa-infer-errors))))

(add-hook 'elsa--infer-if-hook 'elsa-check-if-useless-progn)

(provide 'elsa-check-if)
