;; -*- lexical-binding: t -*-

(require 'elsa-reader)
(require 'elsa-rules-list)
(require 'elsa-check)
(require 'elsa-infer)
(require 'elsa-error)
(require 'elsa-types)
(require 'elsa-english)

(require 'elsa-typed-builtin)

(defun elsa--arglist-to-arity (arglist)
  "Return minimal and maximal number of arguments ARGLIST supports.

If there is a &rest argument we represent the upper infinite
number by symbol 'many."
  (let ((min 0)
        (max 0))
    (cond
     ((eq arglist t)
      (cons 0 'many))
     (t
      (while (and arglist (not (memq (car arglist) '(&optional &rest))))
        (cl-incf min)
        (!cdr arglist))
      (when (eq (car arglist) '&optional)
        (!cdr arglist))
      (setq max min)
      (while (and arglist (not (eq (car arglist) '&rest)))
        (cl-incf max)
        (!cdr arglist))
      (when (eq (car arglist) '&rest)
        (setq max 'many))
      (cons min max)))))

(defun elsa-fn-arity (fn)
  (elsa--arglist-to-arity (help-function-arglist fn)))

(defun elsa--analyse-float (form scope)
  nil)

(defun elsa--analyse-integer (form scope)
  nil)

(defun elsa--analyse-keyword (form scope)
  nil)

(defun elsa--analyse-symbol (form scope)
  (oset form type (elsa--infer-symbol form scope))
  nil)

(defun elsa--analyse-vector (form scope)
  nil)

(defun elsa--analyse-string (form scope)
  nil)

(defun elsa--analyse-let (form scope)
  (let (errors)
    (let ((new-vars nil)
          (bindings (elsa-form-sequence (cadr (oref form sequence))))
          (body (cddr (oref form sequence))))
      ;; TODO: move this to extension?
      (-each bindings
        (lambda (binding)
          (cond
           ((elsa-form-list-p binding)
            (-let [(var source) (oref binding sequence)]
              (if (not source)
                  (push (elsa-variable
                         :name (oref var name) :type (elsa-type-nil))
                        new-vars)
                (push (elsa--analyse-form source scope) errors)
                (push (elsa-variable
                       :name (oref var name) :type (oref source type))
                      new-vars))))
           ((elsa-form-symbol-p binding)
            (push (elsa-variable :name (oref binding name) :type (elsa-make-type nil))
                  new-vars)))))
      (-each new-vars (lambda (v) (elsa-scope-add-variable scope v)))
      (push (--map (elsa--analyse-form it scope) body) errors)
      (oset form type (oref (-last-item body) type))
      (-each new-vars (lambda (v) (elsa-scope-remove-variable scope v))))
    (-flatten errors)))

(defun elsa--analyse-let* (form scope)
  (let (errors)
    (let ((new-vars nil)
          (bindings (oref (cadr (oref form sequence)) sequence))
          (body (cddr (oref form sequence))))
      (-each bindings
        (lambda (binding)
          (let (variable)
            (cond
             ((elsa-form-list-p binding)
              (-let [(var source) (oref binding sequence)]
                (if (not source)
                    (setq variable (elsa-variable
                                    :name (oref var name) :type (elsa-type-nil)))
                  (push (elsa--analyse-form source scope) errors)
                  (setq variable (elsa-variable
                                  :name (oref var name) :type (oref source type))))))
             ((elsa-form-symbol-p binding)
              (setq variable (elsa-variable :name (oref binding name) :type (elsa-make-type nil)))))
            (elsa-scope-add-variable scope variable))))
      (push (--map (elsa--analyse-form it scope) body) errors)
      (oset form type (oref (-last-item body) type))
      (-each new-vars (lambda (v) (elsa-scope-remove-variable scope v))))
    (-flatten errors)))

(defun elsa--analyse-if (form scope)
  (let ((condition (nth 1 (oref form sequence)))
        (true-body (nth 2 (oref form sequence)))
        (false-body (nthcdr 3 (oref form sequence))))
    (-flatten
     (-concat
      (elsa--analyse-form condition scope)
      (elsa--analyse-form true-body scope)
      (when false-body (--map (elsa--analyse-form it scope) false-body))))))

(defun elsa--analyse-progn (form scope)
  (let* ((body (cdr (oref form sequence)))
         (last (-last-item (oref form sequence)))
         (errors (--map (elsa--analyse-form it scope) body)))
    (if body
        (oset form type (oref last type))
      (oset form type (elsa-type-nil)))
    (-flatten errors)))

(defun elsa--analyse-prog1 (form scope)
  (let* ((body (cdr (oref form sequence)))
         (first (car body))
         (errors (--map (elsa--analyse-form it scope) body)))
    (if first
        (oset form type (oref first type))
      (oset form type (elsa-type-unbound)))
    (-flatten errors)))

(defun elsa--analyse-defun (form scope)
  (let* (;; (head (elsa-form-car form))
         ;; (name (oref head name))
         (args (nth 2 (oref form sequence)))
         (body (nthcdr 3 (oref form sequence)))
         ;; (type (get name 'elsa-type))
         (vars))
    (when (elsa-form-list-p args)
      (-each (oref args sequence)
        (lambda (arg)
          (let ((var (elsa-variable
                      :name (elsa-form-name arg)
                      :type (elsa-make-type 'mixed))))
            (push var vars)
            (elsa-scope-add-variable scope var)))))
    (prog1 (-flatten (--map (elsa--analyse-form it scope) body))
      (--each vars (elsa-scope-remove-variable scope it)))))

(defun elsa--analyse-quote (form scope)
  (let ((arg (cadr (oref form sequence))))
    (cond
     ((elsa-form-list-p arg)
      (oset form type (elsa-type-list)))))
  nil)

(defun elsa--analyse-backquote (form scope)
  nil)

(defun elsa--analyse-unquote (form scope)
  nil)

(defun elsa--analyse-splice (form scope)
  nil)

(defun elsa--analyse-function-call (form scope)
  (let* ((errors)
         (head (elsa-form-car form))
         (name (oref head name))
         (args (cdr (oref form sequence)))
         (type (get name 'elsa-type)))
    (push (--map (elsa--analyse-form it scope) args) errors)
    ;; check arity
    (-let (((min . max) (elsa-fn-arity name))
           (num-of-args (length args)))
      (if (< num-of-args min)
          (push
           (elsa-make-error
            (format "Function `%s' expects at least %d %s but received %d"
                    name min
                    (elsa-pluralize "argument" min)
                    num-of-args)
            head)
           errors))
      (if (and (not (eq max 'many))
               (> num-of-args max))
          (push
           (elsa-make-error
            (format "Function `%s' expects at most %d %s but received %d"
                    name max
                    (elsa-pluralize "argument" max)
                    num-of-args)
            head)
           errors)))
    ;; check the types
    (when type
      ;; analyse the arguments
      (cl-mapc
       (lambda (expected actual argument-form index)
         (unless (elsa-type-accept expected actual)
           (push
            (elsa-error
             :message (format "Argument %d accepts type %s but received %s"
                              index
                              (elsa-type-describe expected)
                              (elsa-type-describe actual))
             :expression argument-form
             :line (oref head line)
             :column (oref head column))
            errors)))
       (oref type args)
       (-map (lambda (a) (oref a type)) args)
       args
       (number-sequence 1 (length args)))

      ;; set the return type of the form according to the return type
      ;; of the function's declaration
      (oset form type (oref type return)))
    (-flatten errors)))

(defun elsa--analyse-list (form scope)
  ;; handle special forms
  (let ((head (elsa-form-car form)))
    (when (elsa-form-symbol-p head)
      (let* ((name (oref head name))
             (analyse-fn-name (intern (concat "elsa--analyse-" (symbol-name name)))))
        (pcase name
          ((guard (functionp analyse-fn-name))
           (funcall analyse-fn-name form scope))
          (`\` (elsa--analyse-backquote form scope))
          (`\, (elsa--analyse-unquote form scope))
          (`\,@ (elsa--analyse-splice form scope))
          ;; function call
          (_ (elsa--analyse-function-call form scope)))))))

(defun elsa--analyse-improper-list (form scope)
  nil)

(defun elsa--analyse-form (form scope)
  "Analyse FORM.

FORM is a result of `elsa-read-form'."
  (-non-nil
   (-concat
    (cond
     ((elsa-form-float-p form) (elsa--analyse-float form scope))
     ((elsa-form-integer-p form) (elsa--analyse-integer form scope))
     ((elsa-form-keyword-p form) (elsa--analyse-keyword form scope))
     ((elsa-form-symbol-p form) (elsa--analyse-symbol form scope))
     ((elsa-form-vector-p form) (elsa--analyse-vector form scope))
     ((elsa-form-string-p form) (elsa--analyse-string form scope))
     ((elsa-form-list-p form) (elsa--analyse-list form scope))
     ((elsa-form-improper-list-p form) (elsa--analyse-improper-list form scope))
     (t (error "Invalid form")))
    (-flatten
     (--map (when (elsa-check-should-run it form scope)
              (elsa-check-check it form scope))
            elsa-checks)))))

(provide 'elsa-analyser)
