(require 'paren-face nil t)

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
         (list op (1+ cl) op (1+ op) cl (1+ cl)))
        t))))

(defun elsa--font-lock-reset-anchor ()
  (or (re-search-backward (rx (or "::" "elsa-make-type"))
                          (or (car-safe (nth 9 (syntax-ppss)))
                              (line-beginning-position))
                          t)
      (and (re-search-backward "elsa-declare-defun"
                               (or (car-safe (nth 9 (syntax-ppss)))
                                   (line-beginning-position))
                               t)
           (forward-sexp 3)
           (down-list)
           t)))

(defun elsa--font-lock-setup-anchor (offset)
  (ignore-errors
    (when (save-excursion
            (backward-sexp 2)
            (looking-at-p "elsa-declare-defun"))
      (forward-sexp)))
  (save-excursion (up-list) (+ offset (point))))

(defconst elsa--font-lock-function-anchors
  `(
    ;; highlight the types
    ("\\(\\_<\\(?:\\sw\\|\\s_\\)+\\_>\\)"
     (elsa--font-lock-setup-anchor 0)
     (elsa--font-lock-reset-anchor)
     (0 font-lock-type-face t))
    ;; highlight parens with paren-face if it is defined
    ("[][()]"
     (elsa--font-lock-setup-anchor -1)
     (elsa--font-lock-reset-anchor)
     (0 ,(if (facep 'parenthesis) ''parenthesis nil) t))
    ;; highlight matching pairs of parens for function args
    (elsa--match-function-parens
     (elsa--font-lock-setup-anchor -1)
     (elsa--font-lock-reset-anchor)
     (1 nil t)
     (2 nil t))
    ;; highlight built-in constants
    (,(rx symbol-start (or "nil" "t") symbol-end)
     (elsa--font-lock-setup-anchor -1)
     (elsa--font-lock-reset-anchor)
     (0 font-lock-constant-face t))
    ;; generics
    (,(rx symbol-start
          "&" (group (1+ (or (syntax word) (syntax symbol))))
          symbol-end)
     (elsa--font-lock-setup-anchor -1)
     (elsa--font-lock-reset-anchor)
     (0 font-lock-variable-name-face t))
    (,(rx symbol-start (or "&extends" "&implements") symbol-end)
     (elsa--font-lock-setup-anchor -1)
     (elsa--font-lock-reset-anchor)
     (0 font-lock-keyword-face t))
    ;; highlight keywords and special type constructors
    (,(rx "(" symbol-start (group (or "and" "or" "is" "readonly" "const")) symbol-end)
     (elsa--font-lock-setup-anchor 0)
     nil
     (1 font-lock-keyword-face t))))

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
      ,@elsa--font-lock-function-anchors)
     (,(rx "(" (* space) (group "elsa-declare-defun") (1+ space) (group (1+ (or (syntax word) (syntax symbol)))))
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face)
      ,@elsa--font-lock-function-anchors)
     (,(rx "(" (* space) "elsa-declare-defvar" (1+ space) (group (1+ (or (syntax word) (syntax symbol)))))
      (1 font-lock-variable-name-face))
     (,(rx "(" (* space) "elsa-declare-defclass" (1+ space) (group (1+ (or (syntax word) (syntax symbol)))))
      (1 font-lock-type-face))
     (,(rx "(" (* space) "elsa-declare-defstruct" (1+ space) (group (1+ (or (syntax word) (syntax symbol)))))
      (1 font-lock-type-face)))))

(provide 'elsa-font-lock)
