(require 'elsa-analyser)
(require 'elsa-typed-cl)

(require 'elsa-extension-eieio)

(defun elsa--cl-analyse-specifiers (args-raw)
  "Analyse the cl-style specifiers to extract type information.

ARGS-RAW is the raw forms as present in the `cl-defmethod' or
`cl-defgeneric' arglist."
  (elsa-form-foreach args-raw
    (lambda (arg)
      (when (elsa-form-list-p arg)
        (-when-let* ((arg-form (elsa-car arg))
                     (type-annotation (elsa-cadr arg))
                     (struct-name (elsa-get-name type-annotation))
                     (struct (get struct-name 'elsa-cl-structure)))
          (oset arg-form type (elsa--make-type `(struct ,(oref struct name)))))))))

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
    (elsa--cl-analyse-specifiers args-raw)
    (elsa--analyse-defun-like-form name args body form scope state)))

(defun elsa--analyse:cl-defgeneric (form scope state)
  (elsa--analyse:cl-defmethod form scope state)
  ;; (let ((name (elsa-get-name (elsa-cadr form)))
  ;;       (args (elsa-nth 2 form)))
  ;;   (elsa--cl-analyse-specifiers args-raw)
  ;;   (elsa--analyse-register-functionlike-declaration name args state))
  )

(defun elsa--analyse:cl-defstruct (form scope state)
  (let* ((name (elsa-get-name (elsa-cadr form)))
         (params (when (elsa-form-list-p (elsa-cadr form))
                   (elsa-cdr (elsa-cadr form))))
         (slots (elsa-nthcdr 2 form)))

    (elsa-state-add-structure state
      (elsa-cl-structure
       :name name
       :slots (elsa-eieio--create-slots
               (mapcar
                (lambda (slot)
                  (cond
                   ((elsa-form-symbol-p slot)
                    (list (elsa-get-name slot) :type (elsa-type-mixed)))
                   ((elsa-form-list-p slot)
                    (let* ((slot-name (elsa-get-name slot))
                           (default (elsa-cadr slot))
                           (type (map-elt (elsa-nthcdr 2 slot) :type))
                           (read-only (map-elt (elsa-nthcdr 2 slot) :read-only))
                           (elsa-type (or (elsa--cl-type-to-elsa-type
                                           (elsa-form-to-lisp type))
                                          (elsa-type-mixed)))
                           (elsa-type (if read-only
                                          (elsa-readonly-type :type elsa-type)
                                        elsa-type)))
                      (when (elsa-form-keyword-p default)
                        (elsa-state-add-message state
                          (elsa-make-warning default
                            "Second argument of a slot definition list should be the default value, not an option %s"
                            (elsa-tostring default))))

                      (elsa-state-add-defun state
                        (elsa-defun
                         :name (intern (concat (symbol-name name) "-" (symbol-name slot-name)))
                         :type (elsa-function-type
                                :args (list (elsa--make-type `(struct ,name)))
                                :return elsa-type)
                         :arglist (list '&rest 'args)))

                      (list slot-name :type elsa-type)))))
                slots))
       :parents (list (list name))))

    (elsa-state-add-defun state
      (elsa-defun
       :name (intern (concat (symbol-name name) "-p"))
       :type (elsa--make-type `(function (mixed) (is (struct ,name))))
       :arglist (list 'x)))

    (let ((has-default-constructor t))
      (--each params
        (when-let ((ctr-def (map-elt it :constructor)))
          (if (elsa-get-name ctr-def)
              (elsa-state-add-defun state
                (elsa-defun
                 :name (elsa-get-name ctr-def)
                 :type (elsa--make-type `(function (&rest mixed) (struct ,name)))
                 :arglist (list '&rest 'args)))
            (setq has-default-constructor nil))))

      (when has-default-constructor
        (elsa-state-add-defun state
          (elsa-defun
           :name (intern (concat "make-" (symbol-name name)))
           :type (elsa--make-type `(function (&rest mixed) (struct ,name)))
           :arglist (list '&rest 'args)))))))

(provide 'elsa-extension-cl)
