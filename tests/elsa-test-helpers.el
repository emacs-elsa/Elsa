;; -*- lexical-binding: t -*-
(when (require 'undercover nil t) (undercover))

(require 'elsa-type-helpers)
(require 'elsa-methods)
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
  (let ((state (make-symbol "state")))
    `(elsa-test-with-buffer ,initial
       (let ((,state (elsa-state)))
         (elsa--setup-buffer ,state)
         (let ((,form-var (elsa-read-form ,state)))
           ,@body)))))

(defmacro elsa-test-with-analysed-form (initial form-var &rest body)
  (declare (indent 2))
  (let ((state (or (plist-get body :state-var) (make-symbol "state")))
        (errors (or (plist-get body :errors-var) (make-symbol "errors")))
        (state-value (or (plist-get body :state) '(elsa-state)) ))
    `(elsa-test-with-buffer ,initial
       (let* ((,state ,state-value))
         (elsa--setup-buffer ,state)
         (let ((,form-var (elsa-read-form ,state)))
           (elsa--analyse-form ,form-var (oref ,state scope) ,state)
           (let ((,errors (oref ,state errors)))
             ,@body))))))

(buttercup-define-matcher-for-binary-function :to-be-type-equivalent elsa-type-equivalent-p
  :expect-match-phrase "Expected %A to be equivalent to %B, was %a."
  :expect-mismatch-phrase "Expected %A to not to be equivalent to %B, was %a")

(buttercup-define-matcher-for-binary-function :to-accept-type elsa-type-accept
  :expect-match-phrase "Expected %A to accept %B, was %a."
  :expect-mismatch-phrase "Expected %A to not to accept %B, was %a")

(defun elsa-test--compare-form-print (form result)
  (equal (elsa-tostring form) result))

(buttercup-define-matcher-for-binary-function :to-print-as elsa-test--compare-form-print
  :expect-match-phrase "Expected %A to print as %B, was %a."
  :expect-mismatch-phrase "Expected %A not to print as %B, was %a")

(defun elsa-test--match-error-message (err pattern)
  (string-match-p pattern (oref err message)))

(buttercup-define-matcher-for-binary-function :message-to-match elsa-test--match-error-message
  :expect-match-phrase "Expected error message of %A to match pattern %B, was %a."
  :expect-mismatch-phrase "Expected error message of %A not to match pattern %B, was %a")

(provide 'elsa-test-helpers)
