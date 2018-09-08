(require 'elsa-analyser)
(require 'elsa-typed-cl)

(defun elsa--analyse:cl-defmethod (form scope state)
  (let* ((sequence (oref form sequence))
         (name (elsa-get-name (nth 1 sequence)))
         (args (-map (lambda (arg)
                       (cond
                        ((elsa-form-list-p arg)
                         (elsa-car arg))
                        (t arg)))
                     (oref (nth 2 sequence) sequence)))
         (body (nthcdr 3 sequence)))
    (elsa--analyse-defun-like-form name args body form scope state)))

(defun elsa--analyse:cl-defgeneric (form scope state) nil)

(provide 'elsa-extension-cl)
