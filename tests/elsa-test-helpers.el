(defmacro elsa-test-with-buffer (initial &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,initial)
     (goto-char (point-min))
     (when (re-search-forward "|" nil t)
       (delete-char -1))
     ,@body))

(defmacro elsa-test-with-analysed-form (initial form-var &rest body)
  (declare (indent 2))
  `(elsa-test-with-buffer ,initial
     (let ((,form-var (elsa-read-form)))
       (elsa--analyse-form ,form-var (elsa-scope))
       ,@body)))

(provide 'elsa-test-helpers)
