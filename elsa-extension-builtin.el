(require 'elsa-analyser)
(require 'elsa-infer)

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
     (t (oset form type (elsa-make-type T?))))))

(defun elsa--analyse--eq (eq-form symbol-form constant-form)
  (let ((name (elsa-form-name symbol-form))
        (type))
    (setq type
          (cond
           ((elsa-form-keyword-p constant-form) (elsa-make-type Keyword))
           ((elsa--quoted-symbol-p constant-form) (elsa-make-type Symbol))
           ((and (elsa-form-symbol-p constant-form)
                 (eq (elsa-form-name constant-form) t))
            (elsa-make-type T))
           ((and (elsa-form-symbol-p constant-form)
                 (eq (elsa-form-name constant-form) nil))
            (elsa-make-type Nil))
           ((elsa-form-integer-p constant-form) (elsa-make-type Int))
           ((elsa-form-float-p constant-form) (elsa-make-type Float))))
    (when type (oset eq-form narrow-types (list (elsa-variable :name name :type type))))))

(defun elsa--analyse:eq (form scope state)
  (elsa--analyse-function-call form scope state)
  (let* ((args (cdr (oref form sequence)))
         (first (car args))
         (second (cadr args)))
    (cond
     ((and (elsa-form-symbol-p first)
           (elsa-scope-get-var scope (elsa-form-name first)))
      (elsa--analyse--eq form first second))
     ((and (elsa-form-symbol-p second)
           (elsa-scope-get-var scope (elsa-form-name second)))
      (elsa--analyse--eq form second first)))))

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
  (-when-let* ((car-type (oref (nth 1 (oref form sequence)) type))
               (cdr-type (oref (nth 2 (oref form sequence)) type)))
    (oset form type (elsa-type-cons :car-type car-type :cdr-type cdr-type))))

(defun elsa--analyse:elt (form scope state)
  (elsa--analyse-function-call form scope state)
  (-when-let* ((arg (cadr (oref form sequence)))
               (arg-type (oref arg type)))
    (when (elsa-instance-of arg-type (elsa-make-type Sequence))
      (let* ((item-type (elsa-type-get-item-type arg-type))
             ;; with lists it returns nil when overflowing, otherwise
             ;; throws an error
             (item-type (if (elsa-type-list-p arg-type)
                            (elsa-type-make-nullable item-type)
                          item-type)))
        (oset form type item-type)))))

;; * predicates
(defun elsa--analyse:stringp (form scope state)
  (elsa--analyse-function-call form scope state)
  (oset form type
        (elsa--infer-unary-fn form
          (lambda (arg-type)
            (cond
             ((elsa-type-accept (elsa-type-string) arg-type)
              (elsa-type-t))
             ;; if the arg-type has string as a component, for
             ;; example int | string, then it might evaluate
             ;; sometimes to true and sometimes to false
             ((elsa-type-accept arg-type (elsa-type-string))
              (elsa-make-type T?))
             (t (elsa-type-nil)))))))

(provide 'elsa-extension-builtin)
