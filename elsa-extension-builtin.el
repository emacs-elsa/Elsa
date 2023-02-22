(require 'f)

(require 'elsa-reader)
(require 'elsa-analyser)
(require 'elsa-extension-subr)

(defvar elsa-analyzed nil
  "List of already analyzed files.")

(defun elsa--analyse:require (form scope state)
  (let ((feature (elsa-nth 1 form)))
    (when (elsa--quoted-symbol-p feature)
      (let* ((load-suffixes (list ".el" ".el.gz"))
             (load-file-rep-suffixes (list ""))
             ;; elsa-nth because feature is (quote library)
             (library-symbol (elsa-get-name (elsa-nth 1 feature)))
             (library-name (symbol-name library-symbol))
             (library (locate-library library-name)))
        (push (elsa-get-name (elsa-cadr feature)) (oref state requires))))))

(defun elsa--analyse:provide (form scope state)
  (let ((feature (elsa-nth 1 form)))
    (when (elsa--quoted-symbol-p feature)
      (push (elsa-get-name (elsa-cadr feature)) (oref state provide)))))

(declare-function elsa-process-file "elsa" (file))

;; * boolean functions
(defun elsa--analyse:not (form scope state)
  (elsa--analyse-function-call form scope state)
  (let* ((args (cdr (oref form sequence)))
         (arg-type (oref (car args) type)))
    (cond
     ((elsa-type-accept (elsa-type-nil) arg-type) ;; definitely false
      (oset form type (elsa-type-t)))
     ((not (elsa-type-accept arg-type (elsa-type-nil))) ;; definitely true
      (oset form type (elsa-type-nil)))
     (t (oset form type (elsa-make-type bool))))))

(defun elsa--analyse--eq (eq-form symbol-form constant-form)
  "Setup narrowing for the EQ-FORM based on the SYMBOL-FORM variable.

CONSTANT-FORM is the value to which the variable is narrowed."
  (let ((name (elsa-get-name symbol-form))
        (type))
    (setq type
          ;; TODO: can't we check here that it is `const' type?
          (cond
           ((elsa-form-keyword-p constant-form) (elsa-make-type keyword))
           ((elsa--quoted-symbol-p constant-form) (elsa-make-type symbol))
           ((and (elsa-form-symbol-p constant-form)
                 (eq (elsa-get-name constant-form) t))
            (elsa-make-type t))
           ((and (elsa-form-symbol-p constant-form)
                 (eq (elsa-get-name constant-form) nil))
            (elsa-make-type nil))
           ((elsa-form-integer-p constant-form) (elsa-make-type int))
           ((elsa-form-float-p constant-form) (elsa-make-type float))))
    (when type
      (let ((narrow-type
             (if (or (elsa-type-t-p type)
                     (elsa-type-nil-p type))
                 type
               (elsa-const-type :type type
                                :value (cond
                                        ((elsa--quoted-symbol-name constant-form))
                                        ((elsa-get-name constant-form))
                                        ((oref constant-form value)))))))
        (oset eq-form narrow-types
              (list
               (elsa-variable :name name :type narrow-type)))))))

(defun elsa--analyse:eq (form scope state)
  (elsa--analyse-function-call form scope state)
  (let* ((args (elsa-cdr form))
         (first (car args))
         ;; This can be missing if `eq' was found inside a `pcase'
         ;; pattern.
         (second (cadr args)))
    (when (and first second)
      ;; If one or the other argument is a symbol, we will try to
      ;; narrow its type after the `eq' check based on the other
      ;; argument's type.
      (cond
       ((and (elsa-form-symbol-p first)
             (elsa-scope-get-var scope first))
        (elsa--analyse--eq form first second))
       ((and (elsa-form-symbol-p second)
             (elsa-scope-get-var scope second))
        (elsa--analyse--eq form second first)))
      ;; Here we compute the type of the eq form itself.  By default, it
      ;; has a mixed type, so here we only make the type information
      ;; more precise.
      (cond
       ((elsa-type-equivalent-p
         (elsa-type-empty)
         (elsa-type-intersect first second))
        (oset form type (elsa-type-nil)))
       ((and (elsa-const-type-p (elsa-get-type first))
             (elsa-const-type-p (elsa-get-type second))
             (elsa-type-equivalent-p first second))
        (oset form type (elsa-type-t)))))))

;; * list functions
(defun elsa--analyse:car (form scope state)
  (elsa--analyse-function-call form scope state)
  (-when-let* ((arg (cadr (oref form sequence)))
               (arg-type (oref arg type)))
    (cond
     ((elsa-type-list-p arg-type)
      (oset form type (elsa-type-make-nullable (oref arg-type item-type))))
     ((elsa-type-cons-p arg-type)
      (oset form type (oref arg-type car-type))))))

(defun elsa--analyse:cons (form scope state)
  (elsa--analyse-function-call form scope state)
  (-when-let* ((car-type (elsa-get-type (elsa-nth 1 form)))
               (cdr-type (elsa-get-type (elsa-nth 2 form))))
    (oset form type (elsa-type-cons :car-type car-type :cdr-type cdr-type))))

(defun elsa--analyse:elt (form scope state)
  (elsa--analyse-function-call form scope state)
  (-when-let* ((arg (cadr (oref form sequence)))
               (arg-type (oref arg type)))
    (when (elsa-instance-of arg-type (elsa-make-type sequence))
      (let* ((item-type (elsa-type-get-item-type arg-type))
             ;; with lists it returns nil when overflowing, otherwise
             ;; throws an error
             (item-type (if (elsa-type-list-p arg-type)
                            (elsa-type-make-nullable item-type)
                          item-type)))
        (oset form type item-type)))))

;; * control flow
(defun elsa--analyse:when (form scope state)
  (let ((condition (elsa-nth 1 form))
        (body (elsa-nthcdr 2 form))
        (return-type (elsa-type-empty)))
    (elsa--analyse-form condition scope state)
    (elsa-with-reachability state (elsa-type-is-non-nil condition)
      (elsa-save-scope scope
        (elsa-scope-narrow-var scope (oref condition narrow-types))
        (elsa--analyse-body body scope state)))
    (when body
      (setq return-type (oref (-last-item body) type)))
    (when (elsa-type-accept condition (elsa-type-nil))
      (setq return-type (elsa-type-make-nullable return-type))
      (when (elsa-type-accept (elsa-type-nil) condition)
        (setq return-type (elsa-type-nil))))
    (oset form type return-type)))

(defun elsa--analyse:unless (form scope state)
  (let ((condition (elsa-nth 1 form))
        (body (elsa-nthcdr 2 form))
        (return-type (elsa-type-nil)))
    (elsa--analyse-form condition scope state)
    (elsa-with-reachability state (elsa-type-is-nil condition)
      (elsa-save-scope scope
        (elsa-scope-narrow-var scope (oref condition narrow-types)
                               'elsa-variable-diff)
        (elsa--analyse-body body scope state)))
    (if (not (elsa-type-accept condition (elsa-type-nil)))
        (elsa-type-nil)
      (when body
        (setq return-type (oref (-last-item body) type)))
      (unless (elsa-type-equivalent-p (elsa-type-nil) condition)
        (setq return-type (elsa-type-make-nullable return-type))))
    (oset form type return-type)))

;; TODO: verify that the "rest" :args of the defgroup are valid
(defun elsa--analyse:defgroup (form scope state))

;; TODO: reachability
;; This is not always correct but at least we try to bind variables in
;; case the place is a simple symbol.  The logic is handled in
;; `elsa--analyse-variable-from-binding'
(defun elsa--analyse:when-let (form scope state)
  (let ((bindings (elsa-form-sequence (elsa-nth 1 form)))
        (body (cddr (oref form sequence)))
        (vars))
    (--each bindings
      (-when-let (var (elsa--analyse-variable-from-binding it scope state))
        (elsa-scope-add-var scope var)
        (push var vars)))
    (elsa--analyse-body body scope state)
    (--each vars (elsa-scope-remove-var scope it))
    (oset form type (elsa-get-type (-last-item body)))))

(defun elsa--analyse:list (form scope state)
  (let ((items (elsa-cdr form)))
    (elsa--analyse-body items scope state)
    ;; we need to promote constant types, because the chances this
    ;; tuple is of constant type is slim-to-none
    (let* ((types (-map #'elsa-get-type items))
           (types (--map
                   (if (elsa-const-type-p it)
                       (oref it type)
                     it)
                   types)))
      (oset form type (elsa-type-tuple :types types)))))

(defun elsa--analyse:funcall (form scope state)
  (let ((head (elsa-cadr form))
        (args (elsa-nthcdr 2 form)))
    ;; In case head is a variable, this should set its type to the
    ;; callable type it represents indirectly.
    (when head
      (elsa--analyse-form head scope state)
      (elsa--analyse-function-like-invocation
       form t head args scope state))))

(defun elsa--analyse:error (form scope state)
  "Analyse `error' form.

Since error stops the flow of the program, the form's type is
empty, because it has no value."
  (oset form type (elsa-type-empty)))

(provide 'elsa-extension-builtin)
