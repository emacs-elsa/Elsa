(require 'elsa-analyser)
(require 'elsa-check)
(require 'elsa-ruleset)

(require 'find-func)

(defclass elsa-ruleset-elsa (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-elsa))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-car))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-cdr))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-nth))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-form-sequence))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-get-type))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-get-name))
  )

(defclass elsa-check-elsa-prefer-elsa-car (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-elsa-prefer-elsa-car) form scope state)
  (elsa-form-function-call-p form 'car))

(cl-defmethod elsa-check-check ((_ elsa-check-elsa-prefer-elsa-car) form scope state)
  (let* ((car-arg (elsa-nth 1 form)))
    (when (elsa-form-sequence-p car-arg)
      (let ((head (elsa-car car-arg))
            (prop (elsa-nth 2 car-arg)))
        (when (and (eq (elsa-get-name head) 'oref)
                   (eq (elsa-get-name prop) 'sequence))
          (elsa-state-add-message state
            (elsa-make-notice (elsa-car form)
              "Prefer (elsa-car form) to (car (oref form sequence)).")))
        (when (eq (elsa-get-name head) 'elsa-form-sequence)
          (elsa-state-add-message state
            (elsa-make-notice (elsa-car form)
              "Prefer (elsa-car form) to (car (elsa-form-sequence form)).")))))))

(defclass elsa-check-elsa-prefer-elsa-cdr (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-elsa-prefer-elsa-cdr) form scope state)
  (elsa-form-function-call-p form 'cdr))

(cl-defmethod elsa-check-check ((_ elsa-check-elsa-prefer-elsa-cdr) form scope state)
  (let* ((cdr-arg (elsa-nth 1 form)))
    (when (elsa-form-sequence-p cdr-arg)
      (let ((head (elsa-car cdr-arg))
            (prop (elsa-nth 2 cdr-arg)))
        (when (and (eq (elsa-get-name head) 'oref)
                   (eq (elsa-get-name prop) 'sequence))
          (elsa-state-add-message state
            (elsa-make-notice (elsa-car form)
              "Prefer (elsa-cdr form) to (cdr (oref form sequence)).")))
        (when (eq (elsa-get-name head) 'elsa-form-sequence)
          (elsa-state-add-message state
            (elsa-make-notice (elsa-car form)
              "Prefer (elsa-cdr form) to (cdr (elsa-form-sequence form)).")))))))

(defclass elsa-check-elsa-prefer-elsa-nth (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-elsa-prefer-elsa-nth) form scope state)
  (elsa-form-function-call-p form 'nth))

(cl-defmethod elsa-check-check ((_ elsa-check-elsa-prefer-elsa-nth) form scope state)
  (let* ((nth-arg (elsa-nth 2 form)))
    (when (elsa-form-sequence-p nth-arg)
      (let ((head (elsa-car nth-arg))
            (prop (elsa-nth 2 nth-arg)))
        (when (and (eq (elsa-get-name head) 'oref)
                   (eq (elsa-get-name prop) 'sequence))
          (elsa-state-add-message state
            (elsa-make-notice (elsa-car form)
              "Prefer (elsa-nth n form) to (nth n (oref form sequence)).")))
        (when (eq (elsa-get-name head) 'elsa-form-sequence)
          (elsa-state-add-message state
            (elsa-make-notice (elsa-car form)
              "Prefer (elsa-nth n form) to (nth n (elsa-form-sequence form)).")))))))

(defclass elsa-check-elsa-oref (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-elsa-oref) form scope state)
  (elsa-form-function-call-p form 'oref))

(defclass elsa-check-elsa-prefer-elsa-get-type (elsa-check-elsa-oref) ())

(cl-defmethod elsa-check-check ((_ elsa-check-elsa-prefer-elsa-get-type) form scope state)
  (let* ((prop (elsa-get-name (elsa-nth 2 form))))
    (when (eq prop 'type)
      (elsa-state-add-message state
        (elsa-make-notice (elsa-car form)
          "Prefer (elsa-get-type x) to (oref x type).")))))

(defclass elsa-check-elsa-prefer-elsa-form-sequence (elsa-check-elsa-oref) ())

(cl-defmethod elsa-check-check ((_ elsa-check-elsa-prefer-elsa-form-sequence) form scope state)
  (let* ((prop (elsa-get-name (elsa-nth 2 form))))
    (when (eq prop 'sequence)
      (elsa-state-add-message state
        (elsa-make-notice (elsa-car form)
          "Prefer (elsa-form-sequence form) to (oref form sequence).")))))

(defclass elsa-check-elsa-prefer-elsa-get-name (elsa-check-elsa-oref) ())

(cl-defmethod elsa-check-check ((_ elsa-check-elsa-prefer-elsa-get-name) form scope state)
  (let* ((prop (elsa-get-name (elsa-nth 2 form))))
    (when (eq prop 'name)
      (elsa-state-add-message state
        (elsa-make-notice (elsa-car form)
          "Prefer (elsa-get-name %s) to (oref %s name)."
          (elsa-form-print (elsa-cadr form))
          (elsa-form-print (elsa-cadr form)))))))

(defun elsa--analyse:elsa-make-type (form scope state)
  (elsa--analyse-macro form nil scope state))

(defun elsa--analyse:register-extensions (form scope state)
  (-each (elsa-cdr form)
    (lambda (ext)
      (unless (ignore-errors (find-library-name (concat "elsa-extension-" (symbol-name (elsa-get-name ext)))))
        (elsa-state-add-message state
          (elsa-make-error ext
            "Extension %s not found." (symbol-name (elsa-get-name ext))))))))

(defun elsa--analyse:register-ruleset (form scope state)
  (-each (elsa-cdr form)
    (lambda (ruleset)
      (unless (functionp (intern (concat "elsa-ruleset-" (symbol-name (elsa-get-name ruleset)))))
        (elsa-state-add-message state
          (elsa-make-error ruleset
            "Ruleset %s not found." (symbol-name (elsa-get-name ruleset))))))))

(provide 'elsa-extension-elsa)
