;; -*- lexical-binding: t -*-

(require 'elsa-reader)
(require 'elsa-check)
(require 'elsa-error)
(require 'elsa-types)
(require 'elsa-type-helpers)
(require 'elsa-english)
(require 'elsa-state)
(require 'elsa-scope)

(require 'elsa-typed-builtin)

;; (elsa--arglist-to-arity :: List Symbol | T | String -> Cons Int (Int | Symbol))
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

(defun elsa--analyse-float (_form _scope _state)
  nil)

(defun elsa--analyse-integer (_form _scope _state)
  nil)

(defun elsa--analyse-keyword (_form _scope _state)
  nil)

(defun elsa--analyse-symbol (form scope _state)
  (let* ((name (oref form name))
         (type (cond
                ((eq name t) (elsa-make-type T))
                ((eq name nil) (elsa-make-type Nil))
                ((-when-let (var (elsa-scope-get-var scope form))
                   (clone (oref var type))))
                ((get name 'elsa-type-var))
                (t (elsa-make-type Unbound)))))
    (oset form type type)
    (unless (memq name '(t nil))
      (oset form narrow-types
            (list (elsa-variable :name name
                                 :type (elsa-type-make-non-nullable type)))))))

(defun elsa--analyse-vector (_form _scope _state)
  nil)

(defun elsa--analyse-string (_form _scope _state)
  nil)

(defun elsa--analyse-variable-from-binding (binding scope state)
  "Analyze let-like BINDING and return the resulting variable.

The BINDING should have one of the following forms:

- place   ; initial is nil
- (place) ; initial is nil
- (place initial-value)"
  (cond
   ((or (listp binding)
        (elsa-form-list-p binding))
    (-let [(var source) (elsa-form-sequence binding)]
      (when source
        (elsa--analyse-form source scope state))
      (when (elsa-form-symbol-p var)
        (if (not source)
            (elsa-variable
             :name (oref var name) :type (elsa-type-nil))
          (elsa-variable
           :name (oref var name) :type (oref source type))))))
   ((elsa-form-symbol-p binding)
    (elsa-variable :name (oref binding name) :type (elsa-make-type nil)))
   (t nil)))

(defun elsa--analyse:let (form scope state)
  (let ((new-vars nil)
        (bindings (elsa-form-sequence (elsa-cadr form)))
        (body (cddr (oref form sequence))))
    ;; TODO: move this to extension?
    (-each bindings
      (lambda (binding)
        (push (elsa--analyse-variable-from-binding binding scope state) new-vars)))
    (-each new-vars (lambda (v) (elsa-scope-add-var scope v)))
    (if (not body)
        (oset form type (elsa-type-nil))
      (elsa--analyse-body body scope state)
      (oset form type (oref (-last-item body) type)))
    (-each new-vars (lambda (v) (elsa-scope-remove-var scope v)))))

(defun elsa--analyse:let* (form scope state)
  (let ((new-vars nil)
        (bindings (oref (cadr (oref form sequence)) sequence))
        (body (cddr (oref form sequence))))
    (-each bindings
      (lambda (binding)
        (let ((variable (elsa--analyse-variable-from-binding binding scope state)))
          (push variable new-vars)
          (elsa-scope-add-var scope variable))))
    (if (not body)
        (oset form type (elsa-type-nil))
      (elsa--analyse-body body scope state)
      (oset form type (oref (-last-item body) type)))
    (-each new-vars (lambda (v) (elsa-scope-remove-var scope v)))))

(defun elsa--analyse:if (form scope state)
  (let ((condition (elsa-nth 1 form))
        (true-body (elsa-nth 2 form))
        (false-body (elsa-nthcdr 3 form))
        (mutated-vars-true nil)
        (mutated-vars-false nil))
    (elsa--analyse-form condition scope state)
    (elsa-with-reachability state (elsa-type-is-non-nil condition)
      (elsa-save-scope scope
        (elsa-scope-narrow-var scope (oref condition narrow-types))
        (elsa--analyse-form true-body scope state)
        (setq mutated-vars-true (elsa-scope-get-assigned-vars scope))))
    (elsa-with-reachability state (elsa-type-is-nil condition)
      (elsa-save-scope scope
        (elsa-scope-narrow-var scope (oref condition narrow-types)
                               'elsa-variable-diff)
        (elsa--analyse-body false-body scope state)
        (setq mutated-vars-false (elsa-scope-get-assigned-vars scope))))
    (let ((condition-is-nil (elsa-type-is-nil condition))
          (to-merge))
      (cond
       ((trinary-true-p condition-is-nil)
        (setq to-merge (--each (-map 'clone mutated-vars-false)
                         (oset it assigned (trinary-maybe)))))
       ((trinary-false-p condition-is-nil)
        (setq to-merge (--each (-map 'clone mutated-vars-true)
                         (oset it assigned (trinary-maybe)))))
       ((trinary-maybe-p condition-is-nil)
        (-let (((true false both) (elsa--variables-partition mutated-vars-true mutated-vars-false)))
          (-each (-concat true false)
            (lambda (var)
              (push (elsa-variable
                     :name (elsa-get-name var)
                     :type (-if-let (scope-var (elsa-scope-get-var scope var))
                               (elsa-type-sum var scope-var)
                             (clone (elsa-get-type var)))
                     :assigned (trinary-add-maybe (oref var assigned))
                     :read (trinary-add-maybe (oref var read)))
                    to-merge)))
          (setq to-merge (-concat both to-merge)))))
      (elsa-variables-merge-to-scope to-merge scope))
    (let ((true-result-type (elsa-get-type true-body))
          (false-result-type (if false-body
                                 (elsa-get-type (-last-item false-body))
                               (elsa-type-nil))))
      (oset form type
            (cond
             ((trinary-true-p (elsa-type-is-non-nil condition))
              true-result-type)
             ((trinary-true-p (elsa-type-is-nil condition))
              false-result-type)
             (t (elsa-type-sum true-result-type false-result-type)))))))

(defun elsa--analyse:setq (form scope state)
  (let* ((args (elsa-cdr form))
         (assignments (-partition 2 args)))
    (-each assignments
      (lambda (assignment)
        (let* ((place (car assignment))
               (val (cadr assignment))
               (var (elsa-scope-get-var scope place))
               (special-var (get (elsa-get-name place) 'elsa-type-var)))
          (elsa--analyse-form val scope state)
          (elsa-scope-assign-var scope
            (elsa-variable :name (elsa-get-name place)
                           :type (oref val type)))
          (unless (or var special-var)
            (elsa-state-add-message state
              (elsa-make-warning place
                "Assigning to free variable %s"
                (symbol-name (elsa-get-name place)))))
          (-when-let (type (or
                            (and var (elsa-get-type var))
                            special-var))
            (if (elsa-readonly-type-p type)
                (elsa-state-add-message state
                  (elsa-make-error place
                    "Assignment to read-only variable %s"
                    (symbol-name (elsa-get-name place))))
              (let ((var-type (cond
                               ((elsa-const-type-p type)
                                (oref type type))
                               ((elsa-type-nil-p type)
                                (elsa-type-mixed))
                               (t type))))
                (unless (elsa-type-accept var-type val)
                  (elsa-state-add-message state
                    (elsa-make-error place
                      "Variable %s expects %s, got %s"
                      (symbol-name (elsa-get-name place))
                      (elsa-type-describe var-type)
                      (elsa-type-describe (elsa-get-type val)))))))))))
    (oset form type (oref (-last-item args) type))
    (oset form narrow-types (oref (-last-item args) narrow-types))))

(defun elsa--analyse:cond (form scope state)
  (let ((branches (cdr (oref form sequence)))
        (return-type (elsa-type-empty))
        (condition-reachable (trinary-true)))
    (elsa-save-scope scope
      (-each branches
        (lambda (branch)
          (let* ((branch-seq (elsa-form-sequence branch))
                 (head (car branch-seq))
                 (body (cdr branch-seq)))
            (elsa-with-reachability state condition-reachable
              (when head
                (elsa-save-scope scope
                  (elsa--analyse-form head scope state)
                  (elsa-scope-narrow-var scope (oref head narrow-types))
                  (elsa-with-reachability state (elsa-type-is-non-nil head)
                    (elsa--analyse-body body scope state)))
                (elsa-scope-narrow-var scope (oref head narrow-types)
                                       'elsa-variable-diff)
                (when (trinary-possible-p condition-reachable)
                  (setq return-type
                        (elsa-type-sum return-type (-last-item branch-seq))))
                (setq condition-reachable
                      (trinary-and
                       condition-reachable
                       (elsa-type-is-nil head)))))))))
    (when (trinary-possible-p condition-reachable)
      (setq return-type (elsa-type-make-nullable return-type)))
    (oset form type return-type)))

(defun elsa--analyse:condition-case (form scope state)
  (let* ((seq (oref form sequence))
         (var (nth 1 seq))
         (body (nth 2 seq))
         (handlers (nthcdr 3 seq))
         (return-type))
    (elsa--analyse-form body scope state)
    (setq return-type (oref body type))
    (unless (eq (elsa-get-name var) 'nil)
      (elsa-scope-add-var scope
        (elsa-variable :name (elsa-get-name var))))
    (--each handlers
      (elsa--analyse-form it scope state)
      (let ((last (-last-item (elsa-form-sequence it))))
        (setq return-type (elsa-type-sum return-type last))))
    (unless (eq (elsa-get-name var) 'nil)
      (elsa-scope-remove-var scope var))
    (oset form type return-type)))

(defun elsa--analyse:unwind-protect (form scope state)
  (let* ((seq (oref form sequence))
         (body (nth 1 seq))
         (unwind-forms (nthcdr 2 seq))
         (return-type))
    (elsa--analyse-form body scope state)
    (setq return-type (oref body type))
    (elsa--analyse-body unwind-forms scope state)
    (let ((last (-last-item unwind-forms)))
      (setq return-type (elsa-type-sum return-type last))
      (-when-let (grouped (elsa-variables-group-and-sum
                           (-non-nil
                            (-concat
                             (oref body narrow-types)
                             (oref last narrow-types)))))
        (oset form narrow-types grouped)))
    (oset form type return-type)))

(defun elsa--analyse:progn (form scope state)
  (let* ((body (elsa-cdr form))
         (last (-last-item (elsa-form-sequence form))))
    (elsa--analyse-body body scope state)
    (if body
        (progn
          (oset form type (elsa-get-type last))
          (oset form narrow-types (oref last narrow-types)))
      (oset form type (elsa-type-nil)))))

(defun elsa--analyse:save-excursion (form scope state)
  (elsa--analyse:progn form scope state))

(defun elsa--analyse:save-restriction (form scope state)
  (elsa--analyse:progn form scope state))

(defun elsa--analyse:save-current-buffer (form scope state)
  (elsa--analyse:progn form scope state))

(defun elsa--analyse:track-mouse (form scope state)
  (elsa--analyse:progn form scope state))

(defun elsa--analyse:prog1 (form scope state)
  (let* ((body (elsa-cdr form))
         (first (car body)))
    (elsa--analyse-body body scope state)
    (if first
        (progn
          (oset form type (elsa-get-type first))
          (oset form narrow-types (oref first narrow-types)))
      (oset form type (elsa-type-unbound)))))

(defun elsa--analyse:prog2 (form scope state)
  (let* ((body (elsa-cdr form))
         (second (cadr body)))
    (elsa--analyse-body body scope state)
    (if second
        (progn
          (oset form type (elsa-get-type second))
          (oset form narrow-types (oref second narrow-types)))
      (oset form type (elsa-type-unbound)))))

;; TODO: add reachability analysis
(defun elsa--analyse:or (form scope state)
  (let* ((body (elsa-cdr form))
         (return-type (elsa-type-nil))
         (can-be-nil-p t))
    (elsa-save-scope scope
      (-each body
        (lambda (arg)
          (elsa--analyse-form arg scope state)
          (when can-be-nil-p
            (setq return-type (elsa-type-sum return-type (oref arg type))))
          (unless (elsa-type-accept arg (elsa-type-nil))
            (setq can-be-nil-p nil))
          (elsa-scope-narrow-var scope (oref arg narrow-types)
                                 'elsa-variable-diff))))
    (-when-let (grouped (elsa-variables-group-and-sum
                         (-non-nil (--mapcat (oref it narrow-types) body))))
      (oset form narrow-types grouped))
    (unless can-be-nil-p
      (setq return-type (elsa-type-make-non-nullable return-type)))
    (oset form type return-type)))

;; TODO: add tests for reachability
(defun elsa--analyse:and (form scope state)
  (let* ((body (elsa-cdr form))
         (return-type (elsa-type-t))
         (condition-reachable (trinary-true)))
    (elsa-save-scope scope
      (-each body
        (lambda (arg)
          (elsa-with-reachability state condition-reachable
            (elsa--analyse-form arg scope state)
            (elsa-scope-narrow-var scope (oref arg narrow-types)
                                   'elsa-variable-intersect)
            (setq condition-reachable
                  (trinary-and
                   condition-reachable
                   (elsa-type-is-non-nil arg)))))))
    (-when-let (grouped
                (elsa-variables-group-and-intersect
                 (->> body
                      (--filter (trinary-possible-p (elsa-form-reachable it)))
                      (--mapcat (oref it narrow-types))
                      (-non-nil))))
      (oset form narrow-types grouped))
    (cond
     ((trinary-false-p condition-reachable)
      (setq return-type (elsa-type-nil)))
     ((-any-p
       (lambda (var) (elsa-type-equivalent-p (oref var type) (elsa-type-empty)))
       (oref form narrow-types))
      (setq return-type (elsa-type-nil)))
     (body
      (setq return-type (elsa-type-make-nullable (oref (-last-item body) type)))))
    (oset form type return-type)))

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
  (let* (;; TODO: there should be an api for `(get name
         ;; 'elsa-type)'... probably on `scope', but for now scope is
         ;; separate for each processed file which is not great.
         (function-type (get name 'elsa-type))
         (arg-types (or (elsa-type-get-args function-type)
                        (elsa--get-default-function-types
                         (-map 'elsa-get-name args))))
         (vars))
    (when args
      (-each-indexed (--remove
                      (memq (elsa-get-name it) '(&rest &optional))
                      args)
        (lambda (index arg)
          (let ((var (elsa-variable
                      :name (elsa-get-name arg)
                      :type (nth index arg-types))))
            (push var vars)
            (elsa-scope-add-var scope var)))))
    (when body (elsa--analyse-body body scope state))
    ;; check if return type of defun corresponds with the last form of
    ;; the body
    (let* ((body-return-type (if body (oref (-last-item body) type) (elsa-type-nil)))
           (function-return-type (elsa-type-get-return function-type)))
      (if function-return-type
          (unless (elsa-type-accept function-return-type body-return-type)
            (elsa-state-add-message state
              (elsa-make-error (elsa-car form)
                "Function is expected to return %s but returns %s."
                (elsa-type-describe function-return-type)
                (elsa-type-describe body-return-type))))
        ;; infer the type of the function
        (put name 'elsa-type (elsa-function-type
                              :args arg-types
                              :return body-return-type))))
    (--each vars (elsa-scope-remove-var scope it))))

(defun elsa--analyse:defun (form scope state)
  (let* ((sequence (elsa-form-sequence form))
         (name (elsa-get-name (nth 1 sequence)))
         (args (elsa-form-sequence (nth 2 sequence)))
         (body (nthcdr 3 sequence)))
    (elsa--analyse-defun-like-form name args body form scope state)))

(defun elsa--analyse:defvar (form scope state)
  "Analyze `defvar'.

We infer the type of the variable by the type of the default
form.  This might not always make sense, for example if the value
is `nil' we will infer the type to be always nil, but it might be
a list or something else.

The user can provide a type annotation over the `defvar' form to
make it explicit and precise."
  (let* ((name (elsa-nth 1 form))
         (value (elsa-nth 2 form))
         (var-name (elsa-get-name name))
         (var-type (get var-name 'elsa-type-var)))
    (unless var-type
      (if value
          (progn
            (elsa--analyse-form value scope state)
            (put var-name 'elsa-type-var (oref value type)))
        (put var-name 'elsa-type-var (elsa-make-type Unbound))))))

(defun elsa--analyse:defcustom (form scope state)
  "Analyze `defcustom'.

The analysis works the same way as `elsa--analyse:defvar' except
we take the :type property of the defcustom into account when
automatically deriving the type."
  (let* ((name (elsa-nth 1 form))
         (value (elsa-nth 2 form))
         (var-name (elsa-get-name name))
         (var-type (get var-name 'elsa-type-var)))
    (unless var-type
      (if value
          (progn
            (elsa--analyse-form value scope state)
            ;; TODO: check the `:type' form here and also compare if we
            ;; are doing a valid assignment.
            (put var-name 'elsa-type-var (oref value type)))
        (put var-name 'elsa-type-var (elsa-make-type Unbound))))))

(defun elsa--analyse:defconst (form scope state)
  "Analyze `defconst'.

If no type annotation is provided, find the value type through
`elsa--analyse:defvar' and wrap it as read-only."
  (let* ((name (elsa-nth 1 form))
         (var-name (elsa-get-name name)))
    (unless (get var-name 'elsa-type-var)
      (elsa--analyse:defvar form scope state)
      (put var-name 'elsa-type-var (elsa-readonly-type :type (get var-name 'elsa-type-var))))))

(defun elsa--analyse:defsubst (form scope state)
  (elsa--analyse:defun form scope state))

(defun elsa--analyse:lambda (form scope state)
  (let* ((sequence (oref form sequence))
         (args (nth 1 sequence))
         (body (nthcdr 2 sequence))
         ;; TODO: this should use `elsa--get-default-function-types'
         (arg-types (-repeat (length (elsa-form-sequence args))
                             (elsa-make-type Mixed)))
         (vars))
    (when (elsa-form-list-p args)
      (-each-indexed (elsa-form-sequence args)
        (lambda (index arg)
          (let ((var (elsa-variable
                      :name (elsa-get-name arg)
                      :type (nth index arg-types))))
            (push var vars)
            (elsa-scope-add-var scope var)))))
    (elsa--analyse-body body scope state)
    (--each vars (elsa-scope-remove-var scope it))
    (oset form type (elsa-function-type
                     :args arg-types
                     :return (oref (-last-item body) type)))))

(defun elsa--analyse:quote (form _scope _state)
  (let ((arg (cadr (oref form sequence))))
    (cond
     ((elsa-form-list-p arg)
      (oset form type (elsa-type-list)))
     ((elsa-form-symbol-p arg)
      (oset form type
            (elsa-const-type
             :type (elsa-type-symbol)
             :value (elsa--quoted-symbol-name form))))
     ((elsa-form-keyword-p arg)
      (oset form type (elsa-type-keyword)))
     ((elsa-form-string-p arg)
      (oset form type (elsa-type-string)))
     ((elsa-form-integer-p arg)
      (oset form type (elsa-type-int))))))

(defun elsa--analyse--validate-interactive-string (state arg)
  (let ((str (elsa-form-sequence arg))
        (allowed-codes "[abBcCdDefFGikKmMnNpPrsSUvxXzZ]")
        (case-fold-search nil))
    (with-temp-buffer
      (save-excursion (insert str))
      (skip-chars-forward "@^*")
      (insert "\n")
      (backward-char 1)
      (while (search-forward "\n" nil t)
        (unless (looking-at-p allowed-codes)
          (elsa-state-add-message state
            (elsa-make-error
             arg
             "Unknown interactive code letter: %c"
             (char-after (point)))))))))

(defun elsa--analyse:interactive (form scope state)
  (let ((arg (elsa-cadr form)))
    (cond
     ((null arg))
     ((elsa-form-string-p arg)
      (elsa--analyse--validate-interactive-string state arg))
     ((elsa-form-list-p arg) (elsa--analyse-list arg scope state))
     (t (elsa-state-add-message state
          (elsa-make-error
           arg
           "Invalid interactive spec, expecting string or list form"))))))

(defun elsa--analyse-backquote (_form _scope _state)
  nil)

(defun elsa--analyse-unquote (_form _scope _state)
  nil)

(defun elsa--analyse-splice (_form _scope _state)
  nil)

(defun elsa--analyse-arg-variable-p (arg)
  "Check if ARG is a variable and return its name."
  (and (elsa-form-symbol-p arg)
       (not (oref arg quote-type))
       (elsa-get-name arg)))

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
  (let* ((head (elsa-car form))
         (name (elsa-get-name head))
         (args (elsa-cdr form))
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
          (elsa-state-add-message state
            (elsa-make-error head
              "Function `%s' expects at least %d %s but received %d"
              name min
              (elsa-pluralize "argument" min)
              num-of-args)))
      (if (and (not (eq max 'many))
               (> num-of-args max))
          (elsa-state-add-message state
            (elsa-make-error head
              "Function `%s' expects at most %d %s but received %d"
              name max
              (elsa-pluralize "argument" max)
              num-of-args))))
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
               (elsa-state-add-message state
                 (elsa-make-error head
                   "Argument %d accepts type %s but received %s"
                   (1+ index)
                   (elsa-type-describe expected)
                   (elsa-type-describe actual))))))
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
              (-when-let (varname (elsa--analyse-arg-variable-p arg))
                (push (elsa-variable :name varname :type narrow-type)
                      form-narrow-types)))))
        (oset form narrow-types form-narrow-types)))))

(defun elsa--analyse-function-call (form scope state)
  (elsa--analyse-macro form t scope state))

(defun elsa--analyse-list (form scope state)
  ;; handle special forms
  (let ((head (elsa-car form)))
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

(defun elsa--analyse-improper-list (_form _scope _state)
  nil)

(defun elsa--analyse-form (form scope state)
  "Analyse FORM.

FORM is a result of `elsa-read-form'."
  (oset form reachable (elsa-state-get-reachability state))
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
