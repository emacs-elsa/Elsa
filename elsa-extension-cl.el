(require 'elsa-analyser)
(require 'elsa-typed-cl)

(defun elsa--analyse:cl-defmethod (form scope state)
  "cl-defmethod is like defun, but there can be extra
\"qualifiers\" after the method name.  These are a keywords or
keyword-sexp pairs."
  (let* ((sequence (oref form sequence))
         (name (elsa-get-name (nth 1 sequence)))
         (args-and-body (-drop-while (-not #'elsa-form-list-p)
                                     (nthcdr 2 sequence)))
         (args (-map (lambda (arg)
                       (cond
                        ((elsa-form-list-p arg)
                         (elsa-car arg))
                        (t arg)))
                     (elsa-car args-and-body)))
         (body (elsa-cdr args-and-body)))
    (elsa--analyse-defun-like-form name args body form scope state)))

(defun elsa--analyse:cl-defgeneric (form scope state) nil)

(provide 'elsa-extension-cl)
