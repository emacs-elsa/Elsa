(require 'elsa-error)
(require 'elsa-infer)
(require 'elsa-types)

(defvar elsa--infer-let-hook nil)

(defun elsa--infer-let-run-hooks (orig-fn form scope decls)
  (let ((result (funcall orig-fn form scope decls)))
    (run-hook-with-args 'elsa--infer-let-hook result scope decls)
    result))

(advice-add 'elsa--infer-let :around 'elsa--infer-let-run-hooks)

(provide 'elsa-check-let)
