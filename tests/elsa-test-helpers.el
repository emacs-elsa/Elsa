(require 'elsa-type-helpers)
(require 'buttercup)

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
     (let ((,form-var (elsa-read-form (elsa-scope))))
       ,@body)))

(defmacro elsa-test-with-analysed-form (initial form-var &rest body)
  (declare (indent 2))
  (let ((state (or (plist-get body :state-var) (make-symbol "state")))
        (errors (or (plist-get body :errors-var) (make-symbol "errors"))))
    `(elsa-test-with-buffer ,initial
       (let ((,form-var (elsa-read-form (elsa-scope)))
             (,state (elsa-state)))
         (elsa--analyse-form ,form-var (oref ,state scope) ,state)
         (let ((,errors (oref ,state errors)))
           ,@body)))))

(buttercup-define-matcher-for-binary-function :to-be-type-equivalent elsa-type-equivalent-p
  :expect-match-phrase "Expected %A to be equivalent to %B, was %a."
  :expect-mismatch-phrase "Expected %A to not to be equivalent to %B, was %a")

(buttercup-define-matcher-for-binary-function :to-accept-type elsa-type-accept
  :expect-match-phrase "Expected %A to accept %B, was %a."
  :expect-mismatch-phrase "Expected %A to not to accept %B, was %a")

(defun elsa-test--compare-form-print (form result)
  (equal (elsa-form-print form) result))

(buttercup-define-matcher-for-binary-function :to-print-as elsa-test--compare-form-print
  :expect-match-phrase "Expected %A to print as %B, was %a."
  :expect-mismatch-phrase "Expected %A not to print as %B, was %a")

(provide 'elsa-test-helpers)
