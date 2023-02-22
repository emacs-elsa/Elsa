(defun elsa--match-function-parens (limit)
  "Highlight matching pair of parens for function args."
  (when (re-search-forward "function.*?(" limit t)
    (let ((op (1- (point)))
          cl)
      (backward-char)
      (forward-sexp)
      (when (< (point) limit)
        (setq cl (1- (point)))
        (set-match-data
         (list (set-marker (make-marker) op)
               (set-marker (make-marker) (1+ cl))
               (set-marker (make-marker) op)
               (set-marker (make-marker) (1+ op))
               (set-marker (make-marker) cl)
               (set-marker (make-marker) (1+ cl))))
        t))))

;;;###autoload
(defun elsa-setup-font-lock ()
  "Setup additional font-locking for Elsa forms."
  (interactive)
  (font-lock-add-keywords
   nil
   `((,(rx (or
            (and "("
                 (? (group (or "var"))
                    (1+ space))
                 (group (1+ (or (syntax word) (syntax symbol))))
                 (1+ space)
                 (group "::")
                 (or (1+ space) eol))
            (and "(" (group (and "elsa-make-type" (or (1+ space) eol))))))
      ;; optional annotation type
      (1 font-lock-variable-name-face t t)
      ;; the function name in the defun annotation
      (2 font-lock-function-name-face t t)
      ;; the :: token separating the type
      (3 font-lock-variable-name-face t t)
      ;; elsa-make-type for inline macro use
      (4 font-lock-keyword-face nil t)
      ;; highlight the types
      ("\\(\\_<\\(?:\\sw\\|\\s_\\)+\\_>\\)"
       (save-excursion (up-list) (point))
       (re-search-backward (rx (or "::" "elsa-make-type")))
       (0 font-lock-type-face t))
      ;; highlight parens with paren-face if it is defined
      ("[][()]"
       (save-excursion (up-list) (1- (point)))
       (re-search-backward (rx (or "::" "elsa-make-type")))
       (0 ,(if (facep 'parenthesis) ''parenthesis nil) t))
      ;; highlight matching pairs of parens for function args
      (elsa--match-function-parens
       (save-excursion (up-list) (1- (point)))
       (re-search-backward (rx (or "::" "elsa-make-type")))
       (1 nil t)
       (2 nil t))
      ;; highlight built-in constants
      (,(rx symbol-start (or "nil" "t") symbol-end)
       (save-excursion (up-list) (1- (point)))
       (re-search-backward (rx (or "::" "elsa-make-type")))
       (0 font-lock-constant-face t))
      ;; highlight keywords and special type constructors
      (,(rx "(" symbol-start (group (or "and" "or" "is" "readonly" "const")) symbol-end)
       (save-excursion (up-list) (point))
       nil
       (1 font-lock-keyword-face t)))
     (,(rx "(" (* space) "elsa-declare-defun" (1+ space) (group (1+ (or (syntax word) (syntax symbol)))))
      (1 font-lock-function-name-face))
     (,(rx "(" (* space) "elsa-declare-defvar" (1+ space) (group (1+ (or (syntax word) (syntax symbol)))))
      (1 font-lock-variable-name-face))
     (,(rx "(" (* space) "elsa-declare-structure" (1+ space) (group (1+ (or (syntax word) (syntax symbol)))))
      (1 font-lock-type-face)))))

(provide 'elsa-font-lock)
