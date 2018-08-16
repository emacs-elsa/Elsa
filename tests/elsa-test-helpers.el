(defmacro elsa-test-with-buffer (initial &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,initial)
     (goto-char (point-min))
     (when (search-forward "|" nil t)
       (delete-char -1))
     ,@body))

(defmacro elsa-test-with-read-form (initial form-var &rest body)
  (declare (indent 2))
  `(elsa-test-with-buffer ,initial
     (let ((,form-var (elsa-read-form)))
       ,@body)))

(defmacro elsa-test-with-analysed-form (initial form-var &rest body)
  (declare (indent 2))
  (let ((state (if (eq (car body) :state-var)
                   (cadr body)
                 (make-symbol "state"))))
    `(elsa-test-with-buffer ,initial
       (let ((,form-var (elsa-read-form))
             (,state (elsa-state)))
         (elsa--analyse-form ,form-var (oref ,state scope) ,state)
         ,@body))))

(provide 'elsa-test-helpers)
