(require 'elsa-analyser)

(require 'elsa-typed-subr)

(defalias 'elsa--analyse:with-temp-buffer 'elsa--analyse:progn)

(defun elsa--analyse:dolist (form scope state)
  (let* ((binding-form (elsa-cadr form))
         (body (elsa-nthcdr 2 form))
         (var (elsa-car binding-form))
         (iterable (elsa-cadr binding-form)))
    (elsa--analyse-form iterable scope state)
    (elsa-scope-add-var scope
      (elsa-variable
       :name (elsa-get-name var)
       :type (or (elsa-type-get-item-type (elsa-get-type iterable))
                 (elsa-type-mixed))))
    (elsa--analyse-body body scope state)
    (oset form type (elsa-get-type (-last-item body)))
    (elsa-scope-remove-var scope var)))

(provide 'elsa-extension-subr)
