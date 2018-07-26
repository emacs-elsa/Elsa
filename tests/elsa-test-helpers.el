(defmacro elsa-test-with-buffer (initial &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,initial)
     (goto-char (point-min))
     (when (re-search-forward "|" nil t)
       (delete-char -1))
     ,@body))

(provide 'elsa-test-helpers)
