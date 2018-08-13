;;;###autoload
(defun elsa-setup-font-lock ()
  "Setup additional font-locking for Elsa forms."
  (interactive)
  (font-lock-add-keywords
   nil
   `((,(rx (or
            (and (group "(" (1+ (or (syntax word) (syntax symbol)))) (1+ space) (group "::") (1+ space))
            (group (and "elsa-make-type" (1+ space)))))
      (1 font-lock-doc-face t t)
      (2 font-lock-variable-name-face t t)
      (3 font-lock-keyword-face t t)
      ("\\_<\\([A-Z]\\(?:\\sw\\|\\s_\\)*\\)\\_>"
       (save-excursion (up-list) (point))
       (re-search-backward (rx (or "::" "elsa-make-type")))
       (0 font-lock-type-face t))
      ("\\_<\\([a-z]\\(?:\\sw\\|\\s_\\)*\\)\\_>"
       (save-excursion (up-list) (point))
       (re-search-backward (rx (or "::" "elsa-make-type")))
       (0 nil t))
      ("[][()]"
       (save-excursion (up-list) (1- (point)))
       (re-search-backward (rx (or "::" "elsa-make-type")))
       (0 nil t))
      ("[)]"
       (if (nth 4 (syntax-ppss))
           (progn
             (up-list)
             (backward-char 1)
             (1+ (point)))
         (1+ (point)))
       (re-search-backward (rx (or "::" "elsa-make-type")))
       (0 font-lock-doc-face t))
      (,(rx (or "->" "|"))
       (save-excursion (up-list) (point))
       nil
       (0 font-lock-variable-name-face t))))))

(provide 'elsa-font-lock)
