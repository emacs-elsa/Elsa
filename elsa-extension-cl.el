(require 'elsa-analyser)
(require 'elsa-typed-cl)

(defun elsa--analyse:cl-defmethod (form scope state)
  (let* ((sequence (oref form sequence))
         (name (elsa-form-name (nth 1 sequence)))
         (args (-map (lambda (arg)
                       (cond
                        ((elsa-form-list-p arg)
                         (elsa-form-car arg))
                        (t arg)))
                     (oref (nth 2 sequence) sequence)))
         (body (nthcdr 3 sequence)))
    (elsa--analyse-defun-like-form name args body form scope state)))

(provide 'elsa-extension-cl)
