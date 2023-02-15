(require 'elsa-analyser)
(require 'elsa-typed-cl)

(defun elsa--analyse:cl-defmethod (form scope state)
  "cl-defmethod is like defun, but there can be extra
\"qualifiers\" after the method name.  These are a keywords or
keyword-sexp pairs."
  (let* ((name (elsa-get-name (elsa-cadr form)))
         (args-and-body (-drop-while (-not #'elsa-form-list-p)
                                     (elsa-nthcdr 2 form)))
         (args-raw (elsa-car args-and-body))
         (args (elsa-form-map args-raw
                 (lambda (arg)
                   (if (elsa-form-list-p arg)
                       (elsa-car arg)
                     arg))))
         (body (elsa-cdr args-and-body)))
    ;; Here we attach "cl" type information to the arg forms which can
    ;; be used later during the defun analysis.
    (elsa-form-foreach args-raw
      (lambda (arg)
        (when (elsa-form-list-p arg)
          (-when-let* ((arg-form (elsa-car arg))
                       (type-annotation (elsa-cadr arg))
                       (struct-name (elsa-get-name type-annotation))
                       (struct (get struct-name 'elsa-cl-structure)))
            (oset arg-form type (elsa--make-type `(struct ,(oref struct name))))))))
    (elsa--analyse-defun-like-form name args body form scope state)))

(defun elsa--analyse:cl-defgeneric (form scope state)
  (let ((name (elsa-get-name (elsa-cadr form)))
        (args (elsa-nth 2 form)))
    (elsa--analyse-register-functionlike-declaration name args state)))

(defun elsa--analyse:cl-defstruct (form scope state)
  (let* ((name (elsa-get-name (elsa-cadr form)))
         (slots (elsa-nthcdr 2 form))
         (slot-names (mapcar #'elsa-get-name slots)))
    (elsa-state-add-structure state
      (elsa-cl-structure
       :name name
       :slots slot-names
       :parents (list (list name))))))

(provide 'elsa-extension-cl)
