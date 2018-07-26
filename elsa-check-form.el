(require 'elsa-error)
(require 'elsa-infer)
(require 'elsa-types)

(defvar elsa--infer-form-hook nil)

(defun elsa--infer-form-run-hooks (orig-fn form scope decls)
  (let ((result (funcall orig-fn form scope decls)))
    (run-hook-with-args 'elsa--infer-form-hook result scope decls)
    result))

(advice-add 'elsa--infer-form :around 'elsa--infer-form-run-hooks)

(defun elsa-check-form-unbound-variable (expr &optional scope decls)
  (pcase (eieio-object-class expr)
    (`elsa-expression-var
     (when (eq (eieio-object-class (oref expr type)) 'elsa-type-unbound)
       (push (elsa-error
              :expression expr
              :message (format "Unbound variable %s"
                               (oref (oref expr form) name)))
             elsa-infer-errors)))
    ;; (`elsa-expression-app
    ;;  (-map 'elsa-check-form-unbound-variable (oref expr arguments)))
    ;; (`elsa-expression-let
    ;;  (-each (oref expr bindings)
    ;;    (-lambda ((_ ex)) (elsa-check-form-unbound-variable ex)))
    ;;  (-map 'elsa-check-form-unbound-variable (oref expr body)))
    ;; (`elsa-expression-if
    ;;  (elsa-check-form-unbound-variable (oref expr condition))
    ;;  (elsa-check-form-unbound-variable (oref expr then))
    ;;  (-map 'elsa-check-form-unbound-variable (oref expr else)))
    ))

(add-hook 'elsa--infer-form-hook 'elsa-check-form-unbound-variable)

(provide 'elsa-check-form)
