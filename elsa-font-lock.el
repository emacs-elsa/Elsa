;;;###autoload
(defun elsa-setup-font-lock ()
  "Setup additional font-locking for Elsa forms."
  (interactive)
  (font-lock-add-keywords
   nil
   `((,(rx "("
           (group
            (or
             (and "elsa" (1+ space) (group "::"))
             (and "elsa-make-type" (1+ space))
             (and "elsa-make-type-fn" (1+ space)))))
      (1 font-lock-keyword-face t)
      (2 font-lock-variable-name-face t t)
      ("\\_<\\([A-Z]\\(?:\\sw\\|\\s_\\)*\\)\\_>"
       (save-excursion (up-list) (point))
       (progn (search-backward "(elsa"))
       (0 font-lock-type-face t))
      ("\\_<\\([a-z]\\(?:\\sw\\|\\s_\\)*\\)\\_>"
       (save-excursion (up-list) (point))
       (progn (search-backward "(elsa"))
       (0 nil t))
      ("[()]"
       (save-excursion (up-list) (1- (point)))
       (progn (search-backward "(elsa"))
       (0 nil t))
      (,(rx (or "->" "|"))
       (save-excursion (up-list) (point))
       nil
       (0 font-lock-variable-name-face t))))))

(provide 'elsa-font-lock)
