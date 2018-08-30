(require 'elsa-analyser)
(require 'elsa-check)
(require 'elsa-ruleset)

(require 'find-func)

(defclass elsa-ruleset-elsa (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-elsa))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-car))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-cdr))
  )

(defclass elsa-check-elsa-prefer-elsa-car (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-elsa-prefer-elsa-car) form scope state)
  (elsa-form-function-call-p form 'car))

(cl-defmethod elsa-check-check ((_ elsa-check-elsa-prefer-elsa-car) form scope state)
  (let* ((car-arg (elsa-nth 1 form)))
    (when (elsa-form-sequence-p car-arg)
      (let ((head (elsa-car car-arg))
            (prop (elsa-nth 2 car-arg)))
        (when (and (eq (elsa-form-name head) 'oref)
                   (eq (elsa-form-name prop) 'sequence))
          (elsa-state-add-error state
            (elsa-make-notice
             "Prefer (elsa-car form) to (car (oref form sequence))."
             (elsa-car form))))))))

(defclass elsa-check-elsa-prefer-elsa-cdr (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-elsa-prefer-elsa-cdr) form scope state)
  (elsa-form-function-call-p form 'cdr))

(cl-defmethod elsa-check-check ((_ elsa-check-elsa-prefer-elsa-cdr) form scope state)
  (let* ((car-arg (elsa-nth 1 form)))
    (when (elsa-form-sequence-p car-arg)
      (let ((head (elsa-car car-arg))
            (prop (elsa-nth 2 car-arg)))
        (when (and (eq (elsa-form-name head) 'oref)
                   (eq (elsa-form-name prop) 'sequence))
          (elsa-state-add-error state
            (elsa-make-notice
             "Prefer (elsa-cdr form) to (cdr (oref form sequence))."
             (elsa-car form))))))))

(defun elsa--analyse:elsa-make-type (form scope state)
  (elsa--analyse-macro form nil scope state))

(defun elsa--analyse:register-extensions (form scope state)
  (-each (elsa-cdr form)
    (lambda (ext)
      (unless (ignore-errors (find-library-name (concat "elsa-extension-" (symbol-name (elsa-form-name ext)))))
        (elsa-state-add-error state
          (elsa-make-error
           (format "Extension %s not found." (symbol-name (elsa-form-name ext)))
           ext))))))

(defun elsa--analyse:register-ruleset (form scope state)
  (-each (elsa-cdr form)
    (lambda (ruleset)
      (unless (functionp (intern (concat "elsa-ruleset-" (symbol-name (elsa-form-name ruleset)))))
        (elsa-state-add-error state
          (elsa-make-error
           (format "Ruleset %s not found." (symbol-name (elsa-form-name ruleset)))
           ruleset))))))

(provide 'elsa-extension-elsa)
