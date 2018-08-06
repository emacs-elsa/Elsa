(require 'dash)

(require 'elsa-reader)
(require 'elsa-error)
(require 'elsa-check)
(require 'elsa-scope)
(require 'elsa-state)

(defclass elsa-check-if (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-if) form scope state)
  (elsa-form-function-call-p form 'if))

(defclass elsa-check-if-useless-condition (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-useless-condition) form scope state)
  (let ((condition (cadr (oref form sequence))))
    (if (not (elsa-type-accept (oref condition type) (elsa-type-nil)))
        (elsa-state-add-error state
          (elsa-make-warning "Condition always evaluates to true." condition))
      (when (elsa-type-accept (elsa-type-nil) (oref condition type))
        (elsa-state-add-error state
          (elsa-make-warning "Condition always evaluates to false." condition))))))

(defclass elsa-check-if-useless-then-progn (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-useless-then-progn) form scope state)
  (let ((then-body (nth 2 (oref form sequence))))
    (when (and (eq (elsa-form-name then-body) 'progn)
               (= 2 (length (oref then-body sequence))))
      (elsa-state-add-error state
        (elsa-make-notice "Useless `progn' around body of then branch." (elsa-form-car then-body))))))

(defclass elsa-check-if-useless-else-progn (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-useless-else-progn) form scope state)
  (let ((else-body (nth 3 (oref form sequence))))
    (when (eq (elsa-form-name else-body) 'progn)
      (elsa-state-add-error state
        (elsa-make-notice "Useless `progn' around body of else branch." (elsa-form-car else-body))))))

(defclass elsa-check-if-to-when (elsa-check-if) ())

(cl-defmethod elsa-check-check ((_ elsa-check-if-to-when) form scope state)
  (let ((then-body (nth 2 (oref form sequence)))
        (else-body (nth 3 (oref form sequence))))
    (unless else-body
      (when (eq (elsa-form-name then-body) 'progn)
        (elsa-state-add-error state
          (elsa-make-notice "Rewrite `if' as `when' and unwrap the `progn' which is implicit.'" (elsa-form-car form)))))))

(defclass elsa-check-symbol (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-symbol) form scope state)
  (elsa-form-symbol-p form))

(defclass elsa-check-symbol-naming (elsa-check-symbol) ())

(cl-defmethod elsa-check-check ((_ elsa-check-symbol-naming) form scope state)
  (let ((name (symbol-name (elsa-form-name form))))
    (when (string-match-p ".+_" name)
      (elsa-state-add-error state
        (elsa-make-notice "Use lisp-case for naming symbol instead of snake_case." form)))
    (let ((case-fold-search nil))
      (when (string-match-p ".+[a-z][A-Z]" name)
        (elsa-state-add-error state
          (elsa-make-notice "Use lisp-case for naming symbol instead of camelCase." form))))))

(defclass elsa-check-error-message (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-error-message) form scope state)
  (elsa-form-function-call-p form 'error))

(cl-defmethod elsa-check-check ((_ elsa-check-error-message) form scope state)
  (let ((error-message (nth 1 (oref form sequence))))
    (when (elsa-form-string-p error-message)
      (let ((msg (oref error-message sequence)))
        (when (equal (substring msg -1) ".")
          (elsa-state-add-error state
            (elsa-make-notice "Error messages should not end with a period." (elsa-form-car form))))
        (let ((case-fold-search nil))
          (unless (string-match-p "[A-Z]" msg)
            (elsa-state-add-error state
              (elsa-make-notice "Error messages should start with a capital letter." (elsa-form-car form)))))))))

(defclass elsa-check-unbound-variable (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-unbound-variable) form scope state)
  (and (elsa-form-symbol-p form)
       (not (elsa-form-keyword-p form))))

(cl-defmethod elsa-check-check ((_ elsa-check-unbound-variable) form scope state)
  (let* ((name (elsa-form-name form))
         (var (elsa-scope-get-var scope name)))
    (unless (or (eq name 't)
                (eq name 'nil)
                var
                ;; global variable defined in emacs core as a `defvar'
                ;; FIXME: this actually includes the variables bound
                ;; during analysis, so this is not the proper way to
                ;; check.
                (boundp name))
      (elsa-state-add-error state
        (elsa-make-error
         (format "Reference to free variable `%s'." (symbol-name name))
         form)))))

(defclass elsa-check-cond-useless-condition (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-cond-useless-condition) form scope state)
  (elsa-form-function-call-p form 'cond))

(cl-defmethod elsa-check-check ((_ elsa-check-cond-useless-condition) form scope state)
  (let* ((branches (cdr (oref form sequence)))
         (total (length branches)))
    (-each-indexed branches
      (lambda (index branch)
        (let* ((sequence (oref branch sequence))
               (first-item (-first-item sequence)))
          (if (and (not (elsa-type-accept (oref first-item type) (elsa-type-nil)))
                   (< index (1- total)))
              (elsa-state-add-error state
                (elsa-make-warning "Condition always evaluates to true." first-item))
            (when (elsa-type-accept (elsa-type-nil) (oref first-item type))
              (elsa-state-add-error state
                (elsa-make-warning "Condition always evaluates to false." first-item)))))))))

(provide 'elsa-rules-list)
