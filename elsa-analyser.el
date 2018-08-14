;; -*- lexical-binding: t -*-

(require 'elsa-reader)
(require 'elsa-check)
(require 'elsa-infer)
(require 'elsa-error)
(require 'elsa-types)
(require 'elsa-type-helpers)
(require 'elsa-english)
(require 'elsa-state)

(require 'elsa-typed-builtin)

;; (elsa--arglist-to-arity :: List Symbol -> Cons Int (Int | Symbol))
(defun elsa--arglist-to-arity (arglist)
  "Return minimal and maximal number of arguments ARGLIST supports.

If there is a &rest argument we represent the upper infinite
number by symbol 'many."
  (let ((min 0)
        (max 0))
    (cond
     ;; TODO: this is a dumb fallback to basically mixed... -> mixed
     ;; This should be solved once we do the recursive `require' walks.
     ((or (eq arglist t)
          (stringp arglist))
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

;; (elsa-fn-arity :: Symbol -> Cons Int (Int | Symbol))
(defun elsa-fn-arity (fn)
  (elsa--arglist-to-arity (help-function-arglist fn)))

(defun elsa--analyse-float (form scope state)
  nil)

(defun elsa--analyse-integer (form scope state)
  nil)

(defun elsa--analyse-keyword (form scope state)
  nil)

(defun elsa--analyse-symbol (form scope state)
  (oset form type (elsa--infer-symbol form scope))
  nil)

(defun elsa--analyse-vector (form scope state)
  nil)

(defun elsa--analyse-string (form scope state)
  nil)

(defun elsa--analyse-variable-from-binding (binding scope state)
  (cond
   ((elsa-form-list-p binding)
    (-let [(var source) (oref binding sequence)]
      (if (not source)
          (elsa-variable
           :name (oref var name) :type (elsa-type-nil))
        (elsa--analyse-form source scope state)
        (elsa-variable
         :name (oref var name) :type (oref source type)))))
   ((elsa-form-symbol-p binding)
    (elsa-variable :name (oref binding name) :type (elsa-make-type nil)))
   (t "Error while analysing variable binding")))

(defun elsa--analyse:let (form scope state)
  (let ((new-vars nil)
        (bindings (elsa-form-sequence (cadr (oref form sequence))))
        (body (cddr (oref form sequence))))
    ;; TODO: move this to extension?
    (-each bindings
      (lambda (binding)
        (push (elsa--analyse-variable-from-binding binding scope state) new-vars)))
    (-each new-vars (lambda (v) (elsa-scope-add-variable scope v)))
    (--each body (elsa--analyse-form it scope state))
    (oset form type (oref (-last-item body) type))
    (-each new-vars (lambda (v) (elsa-scope-remove-variable scope v)))))

(defun elsa--analyse:let* (form scope state)
  (let ((new-vars nil)
        (bindings (oref (cadr (oref form sequence)) sequence))
        (body (cddr (oref form sequence))))
    (-each bindings
      (lambda (binding)
        (let ((variable (elsa--analyse-variable-from-binding binding scope state)))
          (push variable new-vars)
          (elsa-scope-add-variable scope variable))))
    (--each body (elsa--analyse-form it scope state))
    (oset form type (oref (-last-item body) type))
    (-each new-vars (lambda (v) (elsa-scope-remove-variable scope v)))))

(defun elsa--analyse:if (form scope state)
  (let ((condition (nth 1 (oref form sequence)))
        (true-body (nth 2 (oref form sequence)))
        (false-body (nthcdr 3 (oref form sequence))))
    (elsa--analyse-form condition scope state)
    (--each (oref condition narrow-types)
      (elsa-scope-add-variable scope it))
    (elsa--analyse-form true-body scope state)
    (--each (oref condition narrow-types)
      (elsa-scope-remove-variable scope it))
    (elsa--analyse-body false-body scope state)
    (let ((result-type (oref true-body type)))
      (when false-body
        (setq result-type
              (elsa-type-sum
               result-type
               (oref (-last-item false-body) type))))
      (oset form type result-type))))

(defun elsa--analyse:cond (form scope state)
  (let ((branches (cdr (oref form sequence)))
        return-type
        (can-be-nil-p t))
    (-each branches
      (lambda (branch)
        (--each (oref branch sequence)
          (elsa--analyse-form it scope state))
        (let ((first-item (-first-item (oref branch sequence)))
              (last-item (-last-item (oref branch sequence))))
          (unless (elsa-type-accept (oref first-item type) (elsa-type-nil))
            (setq can-be-nil-p nil))
          (setq
           return-type
           (if (eq return-type nil)
               (clone (oref last-item type))
             (elsa-type-sum
              return-type
              (oref last-item type)))))))
    (when can-be-nil-p
      (setq return-type (elsa-type-make-nullable return-type)))
    (oset form type return-type)))

(defun elsa--analyse:progn (form scope state)
  (let* ((body (cdr (oref form sequence)))
         (last (-last-item (oref form sequence))))
    (--each body (elsa--analyse-form it scope state))
    (if body
        (oset form type (oref last type))
      (oset form type (elsa-type-nil)))))

(defun elsa--analyse:prog1 (form scope state)
  (let* ((body (cdr (oref form sequence)))
         (first (car body)))
    (--each body (elsa--analyse-form it scope state))
    (if first
        (oset form type (oref first type))
      (oset form type (elsa-type-unbound)))))

(defun elsa--get-default-function-types (args)
  "Return a default list of types based on ARGS.

This function skips over special &optional and &rest markers and
collects all the arguments, turns &optional arguments into
nullables and the &rest argument into a variadic."
  (-let (((min . max) (elsa--arglist-to-arity args)))
    (if (eq max 'many)
        (-snoc (-repeat min (elsa-make-type Mixed))
               (elsa-make-type Variadic Mixed))
      (-repeat max (elsa-make-type Mixed)))))

(defun elsa--analyse-defun-like-form (name args body form scope state)
  (let* ((sequence (oref form sequence))
         (function-type (get name 'elsa-type))
         (arg-types (or (elsa-type-get-args function-type)
                        (elsa--get-default-function-types
                         (-map 'elsa-form-name args))))
         (vars))
    (when args
      (-each-indexed (--remove
                      (memq (elsa-form-name it) '(&rest &optional))
                      args)
        (lambda (index arg)
          (let ((var (elsa-variable
                      :name (elsa-form-name arg)
                      :type (nth index arg-types))))
            (push var vars)
            (elsa-scope-add-variable scope var)))))
    (--each body (elsa--analyse-form it scope state))
    ;; check if return type of defun corresponds with the last form of
    ;; the body
    (let* ((body-return-type (oref (-last-item body) type))
           (function-return-type
            (or (elsa-type-get-return function-type)
                (elsa-make-type Mixed))))
      (unless (elsa-type-accept function-return-type body-return-type)
        (elsa-state-add-error state
          (elsa-make-error
           (format "Function is expected to return %s but returns %s."
                   (elsa-type-describe function-return-type)
                   (elsa-type-describe body-return-type))
           (elsa-form-car form)))))
    (--each vars (elsa-scope-remove-variable scope it))))

(defun elsa--analyse:defun (form scope state)
  (let* ((sequence (oref form sequence))
         (name (elsa-form-name (nth 1 sequence)))
         (args (elsa-form-sequence (nth 2 sequence)))
         (body (nthcdr 3 sequence)))
    (elsa--analyse-defun-like-form name args body form scope state)))

(defun elsa--analyse:defsubst (form scope state)
  (elsa--analyse:defun form scope state))

(defun elsa--analyse:lambda (form scope state)
  (let* ((sequence (oref form sequence))
         (args (nth 1 sequence))
         (body (nthcdr 2 sequence))
         (arg-types (-repeat (length (elsa-form-sequence args))
                             (elsa-make-type Mixed)))
         (vars))
    (when (elsa-form-list-p args)
      (-each-indexed (elsa-form-sequence args)
        (lambda (index arg)
          (let ((var (elsa-variable
                      :name (elsa-form-name arg)
                      :type (nth index arg-types))))
            (push var vars)
            (elsa-scope-add-variable scope var)))))
    (--each body (elsa--analyse-form it scope state))
    (--each vars (elsa-scope-remove-variable scope it))
    (oset form type (elsa-function-type
                     :args arg-types
                     :return (oref (-last-item body) type)))))

(defun elsa--analyse:quote (form scope state)
  (let ((arg (cadr (oref form sequence))))
    (cond
     ((elsa-form-list-p arg)
      (oset form type (elsa-type-list)))
     ((elsa-form-symbol-p arg)
      (oset form type (elsa-type-symbol)))
     ((elsa-form-keyword-p arg)
      (oset form type (elsa-type-keyword)))
     ((elsa-form-string-p arg)
      (oset form type (elsa-type-string)))
     ((elsa-form-int-p arg)
      (oset form type (elsa-type-int))))))

(defun elsa--analyse-backquote (form scope state)
  nil)

(defun elsa--analyse-unquote (form scope state)
  nil)

(defun elsa--analyse-splice (form scope state)
  nil)

;; (elsa--analyse-normalize-spec :: Bool | List Bool -> Mixed -> List Bool)
(defun elsa--analyse-normalize-spec (spec form)
  "Normalize evaluation SPEC for FORM."
  (cond
   ((eq spec t)
    (-repeat (1- (length (elsa-form-sequence form))) t))
   ((eq spec nil)
    (-repeat (1- (length (elsa-form-sequence form))) nil))
   ((eq (-last-item spec) 'body)
    (-concat (-butlast spec)
             (-repeat (- (1- (length (elsa-form-sequence form)))
                         (1- (length spec)))
                      t)))
   (t spec)))

;; (elsa--analyse-macro :: Mixed -> Bool | List Bool -> Mixed -> Mixed -> Mixed)
(defun elsa--analyse-macro (form spec scope state)
  (setq spec (elsa--analyse-normalize-spec spec form))
  (let* ((head (elsa-form-car form))
         (name (oref head name))
         (args (cdr (oref form sequence)))
         (type (get name 'elsa-type))
         (narrow-types (get name 'elsa-narrow-types)))
    (-each (-zip args spec)
      (-lambda ((arg . analysep))
        (when analysep
          (elsa--analyse-form arg scope state))))
    ;; check arity
    (-let (((min . max) (elsa-fn-arity name))
           (num-of-args (length args)))
      (if (< num-of-args min)
          (elsa-state-add-error state
            (elsa-make-error
             (format "Function `%s' expects at least %d %s but received %d"
                     name min
                     (elsa-pluralize "argument" min)
                     num-of-args)
             head)))
      (if (and (not (eq max 'many))
               (> num-of-args max))
          (elsa-state-add-error state
            (elsa-make-error
             (format "Function `%s' expects at most %d %s but received %d"
                     name max
                     (elsa-pluralize "argument" max)
                     num-of-args)
             head))))
    ;; check the types
    (when type
      ;; analyse the arguments
      (when (elsa-function-type-p type)
        (cl-mapc
         (lambda (argument-form index)
           (let* ((expected (elsa-function-type-nth-arg index type))
                  (actual
                   ;; In case we have a quoted symbol and the expected
                   ;; type is function, we will take the function
                   ;; definition from the symbol's plist
                   (cond
                    ((and (elsa-function-type-p expected)
                          (elsa--quoted-symbol-p argument-form))
                     (or (get (elsa--quoted-symbol-name argument-form) 'elsa-type)
                         (elsa-type-mixed)))
                    (t (oref argument-form type)))))
             (unless (or (not expected)
                         (elsa-type-accept expected actual))
               (elsa-state-add-error state
                 (elsa-make-error
                  (format "Argument %d accepts type %s but received %s"
                          (1+ index)
                          (elsa-type-describe expected)
                          (elsa-type-describe actual))
                  head)))))
         args
         (number-sequence 0 (1- (length args)))))

      ;; set the return type of the form according to the return type
      ;; of the function's declaration
      (oset form type (elsa-type-get-return type)))

    ;; compute narrowed types of variables in this function if possible
    (when narrow-types
      (let ((form-narrow-types nil))
        (-each-indexed narrow-types
          (lambda (index narrow-type)
            (let ((arg (nth index args)))
              (when (and (elsa-form-symbol-p arg)
                         (not (oref arg quote-type)))
                (-when-let* ((varname (elsa-form-name arg))
                             (var (elsa-scope-get-var scope varname)))
                  (push (elsa-variable :name varname :type narrow-type)
                        form-narrow-types))))))
        (oset form narrow-types form-narrow-types)))))

(defun elsa--analyse-function-call (form scope state)
  (elsa--analyse-macro form t scope state))

(defun elsa--analyse-list (form scope state)
  ;; handle special forms
  (let ((head (elsa-form-car form)))
    (when (elsa-form-symbol-p head)
      (let* ((name (oref head name))
             (analyse-fn-name (intern (concat "elsa--analyse:" (symbol-name name)))))
        (pcase name
          ((guard (functionp analyse-fn-name))
           (funcall analyse-fn-name form scope state))
          (`\` (elsa--analyse-backquote form scope state))
          (`\, (elsa--analyse-unquote form scope state))
          (`\,@ (elsa--analyse-splice form scope state))
          ;; function call
          (_ (elsa--analyse-function-call form scope state)))))))

(defun elsa--analyse-improper-list (form scope state)
  nil)

(defun elsa--analyse-form (form scope state)
  "Analyse FORM.

FORM is a result of `elsa-read-form'."
  (cond
   ((elsa-form-float-p form) (elsa--analyse-float form scope state))
   ((elsa-form-integer-p form) (elsa--analyse-integer form scope state))
   ((elsa-form-keyword-p form) (elsa--analyse-keyword form scope state))
   ((elsa-form-symbol-p form) (elsa--analyse-symbol form scope state))
   ((elsa-form-vector-p form) (elsa--analyse-vector form scope state))
   ((elsa-form-string-p form) (elsa--analyse-string form scope state))
   ((elsa-form-list-p form) (elsa--analyse-list form scope state))
   ((elsa-form-improper-list-p form) (elsa--analyse-improper-list form scope state))
   (t (error "Invalid form")))
  (--each elsa-checks
    (when (elsa-check-should-run it form scope state)
      (elsa-check-check it form scope state))))

(defun elsa--analyse-body (body scope state)
  (--each body (elsa--analyse-form it scope state)))

(provide 'elsa-analyser)
