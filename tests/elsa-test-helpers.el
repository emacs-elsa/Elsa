;; -*- lexical-binding: t -*-
(when (require 'undercover nil t) (undercover))
(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))

(require 'elsa)
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

(buttercup-define-matcher :to-be-type-equivalent (a b)
  (-let* ((((a-expr . a) (b-expr . b)) (mapcar #'buttercup--expr-and-value (list a b)))
          (type-a (elsa-get-type a))
          (type-b (elsa-get-type b))
          (spec (format-spec-make
                 ?A (format "%S" a-expr)
                 ?a (format "%S" a)
                 ?B (format "%S" b-expr)
                 ?b (format "%S" b)
                 ?t (format "%s" (elsa-tostring type-a))
                 ?o (format "%s" (elsa-tostring type-b)))))
    (if (elsa-type-equivalent-p a b)
        (cons t (buttercup-format-spec
                 "Expected `%A' of type `%t' not to be equivalent to `%B' of type `%o'."
                 spec))
      (cons nil (buttercup-format-spec
                 "Expected `%A' of type `%t' to be equivalent to `%B' of type `%o'."
                 spec)))))

(buttercup-define-matcher :to-accept-type (a b)
  (-let* ((((a-expr . a) (b-expr . b)) (mapcar #'buttercup--expr-and-value (list a b)))
          (type-a (elsa-get-type a))
          (type-b (elsa-get-type b))
          (spec (format-spec-make
                 ?A (format "%S" a-expr)
                 ?a (format "%S" a)
                 ?B (format "%S" b-expr)
                 ?b (format "%S" b)
                 ?t (format "%s" (elsa-tostring type-a))
                 ?o (format "%s" (elsa-tostring type-b)))))
    (if (elsa-type-accept a b)
        (cons t (buttercup-format-spec
                 "Expected `%A' of type `%t' not to accept `%B' of type `%o'."
                 spec))
      (cons nil (buttercup-format-spec
                 "Expected `%A' of type `%t' to accept `%B' of type `%o'."
                 spec)))))

(defun elsa-test--compare-form-print (form result)
  (equal (elsa-tostring form) result))

(buttercup-define-matcher-for-binary-function :to-print-as elsa-test--compare-form-print
  :expect-match-phrase "Expected %A to print as %B, was %a."
  :expect-mismatch-phrase "Expected %A not to print as %B, was %a")

(buttercup-define-matcher :message-to-match (a b)
  (-let* ((((a-expr . a) (b-expr . b)) (mapcar #'buttercup--expr-and-value (list a b)))
          (msg (oref a message))
          (pattern b)
          (matches (string-match-p pattern msg))
          (spec (format-spec-make
                 ?A (format "%S" a-expr)
                 ?a (format "%S" a)
                 ?B (format "%S" b-expr)
                 ?b (format "%S" b)
                 ?m (format "%S" msg))))
    (if matches
        (cons t (buttercup-format-spec
                 "Expected error message of `%A' not to match pattern `%B', was %m."
                 spec))
      (cons nil (buttercup-format-spec
                 "Expected error message of `%A' to match pattern `%B', was %m"
                 spec)))))

(provide 'elsa-test-helpers)
