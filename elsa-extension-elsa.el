(require 'find-func)

(require 'elsa-analyser)
(require 'elsa-check)
(require 'elsa-ruleset)
(require 'elsa-types)

(defclass elsa-ruleset-elsa (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-elsa))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-car))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-cdr))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-nth))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-form-sequence))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-get-type))
  (add-to-list 'elsa-checks (elsa-check-elsa-prefer-elsa-get-name))
  (add-to-list 'elsa-checks (elsa-check-elsa-class-slot-has-type))
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
  (let* ((instance-form (elsa-form-print (elsa-nth 1 form)))
         (prop (elsa-get-name (elsa-nth 2 form))))
    (when (eq prop 'type)
      (elsa-state-add-message state
        (elsa-make-notice (elsa-car form)
          (format "Prefer (elsa-get-type %s) to (oref %s type)."
                  instance-form
                  instance-form))))))

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

(defclass elsa-check-elsa-class-slot-has-type (elsa-check) ())

(cl-defmethod elsa-check-should-run ((_ elsa-check-elsa-class-slot-has-type) form scope state)
  (elsa-form-function-call-p form 'defclass))

(cl-defmethod elsa-check-check ((_ elsa-check-elsa-class-slot-has-type) form scope state)
  (let* ((slots (elsa-nth 3 form)))
    (elsa-form-foreach slots
      (lambda (slot)
        (when (and (elsa-form-list-p slot)
                   (let ((props (elsa-cdr slot)))
                     (->> props
                          (-partition 2)
                          (mapcar #'car)
                          (--none? (eq (elsa-get-name it) :type)))))
          (elsa-state-add-message state
            (elsa-make-notice (elsa-car slot)
              "Every class slot should have a type"
              :code "elsa-class-slot-has-type")))))))

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

(defun elsa--analyse:elsa-declare-defun (form scope state)
  nil)

(defun elsa--analyse:elsa-declare-structure (form scope state)
  nil)

(defun elsa--analyse:elsa-with-temp-explainer (form scope state)
  (let* ((explainer (elsa-cadr form))
         (body (elsa-nthcdr 2 form))
         (last (-last-item body))
         (var (elsa-variable
               :name (elsa-get-name explainer)
               :type (elsa-make-type (class elsa-explainer)))))
    (elsa-scope-add-var scope var)
    (elsa--analyse-body body scope state)
    (elsa-scope-remove-var scope var)
    (if body
        (progn
          (oset form type (elsa-get-type last))
          (oset form narrow-types (oref last narrow-types)))
      (oset form type (elsa-type-nil)))))

(defun elsa--analyse:elsa-with-explainer (form scope state)
  (let* ((explainer (elsa-cadr form))
         (body (elsa-nthcdr 3 form))
         (last (-last-item body)))
    (elsa--analyse-body body scope state)
    (if body
        (progn
          (oset form type (elsa-get-type last))
          (oset form narrow-types (oref last narrow-types)))
      (oset form type (elsa-type-nil)))))

(provide 'elsa-extension-elsa)
