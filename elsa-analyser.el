;; -*- lexical-binding: t -*-

(require 'elsa-reader)
(require 'elsa-check)
(require 'elsa-error)
(require 'elsa-types)
(require 'elsa-type-helpers)
(require 'elsa-type-algebra)
(require 'elsa-english)
(require 'elsa-state)
(require 'elsa-scope)
(require 'elsa-functions)

(require 'elsa-typed-builtin)

;; (elsa--arglist-to-arity :: (function ((or (list (or symbol (cons symbol mixed))) t string)) (cons int (or int (const many) (const undefined)))))
(defun elsa--arglist-to-arity (arglist)
  "Return minimal and maximal number of arguments ARGLIST supports.

If there is a &rest argument we represent the upper infinite
number by symbol 'many.

If the arglist is `t', use a fallback 0 to infinity and return
'undefined for max.  This has the same semantics as 'many but
adds the information that a fallback was used."
  (let ((min 0)
        (max 0))
    (cond
     ;; TODO: this is a dumb fallback to basically mixed... -> mixed
     ;; This should be solved once we do the recursive `require' walks.
     ((or (eq arglist t) (stringp arglist))
      (cons 0 'undefined))
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

;; (elsa-fn-arity :: (function ((struct elsa-state) (or (list (or symbol (cons symbol mixed))) symbol)) (cons int (or int (const many) (const unevalled) (const undefined)))))
(defun elsa-fn-arity (state def)
  (cond
   ;; directly provided arglist
   ((listp def)
    (elsa--arglist-to-arity def))
   ;; symbol can point to a lambda, closure or subr
   ((symbolp def)
    (cond
     ((elsa-state-get-defun state def)
      (let ((dfn (elsa-state-get-defun state def)))
        (if (slot-boundp dfn 'arglist)
            (elsa--arglist-to-arity (oref (elsa-state-get-defun state def) arglist))
          (elsa--arglist-to-arity t))))
     ((indirect-function def)
      (let ((fn (indirect-function def)))
        (cond
         ((subrp fn) (subr-arity fn))
         ((and (byte-code-function-p fn) (integerp (aref fn 0)))
          (func-arity fn))
         ((eq (car-safe fn) 'lambda)
          (elsa--arglist-to-arity (nth 1 fn)))
         ((eq (car-safe fn) 'closure)
          (elsa--arglist-to-arity (nth 2 fn)))
         (t (elsa--arglist-to-arity t)))))
     ;; fallback
     (t (elsa--arglist-to-arity t))))))

(defun elsa--analyse-float (_form _scope _state)
  nil)

(defun elsa--analyse-integer (_form _scope _state)
  nil)

(defun elsa--analyse-keyword (_form _scope _state)
  nil)

;;    (elsa--analyse-symbol :: (function ((struct elsa-form-symbol) mixed (struct elsa-state)) mixed))
(defun elsa--analyse-symbol (form scope state)
  (let* ((name (oref form name))
         (type (cond
                ((elsa-state-quoted-p state)
                 (elsa-const-type
                  :type (elsa-type-symbol)
                  :value name))
                ((eq name t) (elsa-make-type t))
                ((eq name nil) (elsa-make-type nil))
                ((-when-let (var (elsa-scope-get-var scope form))
                   (clone (oref var type))))
                ((and (elsa-state-get-defvar state name)
                      (elsa-get-type (elsa-state-get-defvar state name))))
                (t (elsa-make-type unbound)))))
    (oset form type type)
    (unless (or (memq name '(t nil))
                (elsa-state-quoted-p state))
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
  (let* ((new-vars nil)
         (bindings-maybe (elsa-cadr form))
         (bindings
          ;; If the binding list argument to let is not a list, we are
          ;; probably dealing with some pcase macro or another special
          ;; use-case.  So we simply skip the binding.  It can also be
          ;; empty (that is nil).
          (when (elsa-form-sequence-p bindings-maybe)
            (elsa-form-sequence bindings-maybe)))
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
  (let* ((new-vars nil)
         (bindings-maybe (elsa-cadr form))
         (bindings
          ;; If the binding list argument to let is not a list, we are
          ;; probably dealing with some pcase macro or another special
          ;; use-case.  So we simply skip the binding.  It can also be
          ;; empty (that is nil).
          (when (elsa-form-sequence-p bindings-maybe)
            (elsa-form-sequence bindings-maybe)))
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

;; TODO: add analysis of the reachability of body
(defalias 'elsa--analyse:when-let* 'elsa--analyse:let*)

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
               (special-var (elsa-state-get-defvar state (elsa-get-name place)))
               (special-var-type (and special-var (elsa-get-type special-var))))
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
                            special-var-type))
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
                      "Variable %s expects `%s', got `%s'"
                      (symbol-name (elsa-get-name place))
                      (elsa-type-describe var-type)
                      (elsa-type-describe (elsa-get-type val)))))))))))
    (oset form type (oref (-last-item args) type))
    (oset form narrow-types
          (list (elsa-variable :name (elsa-get-name (car (-last-item assignments)))
                               :type (elsa-type-make-non-nullable
                                      (elsa-get-type (cadr (-last-item assignments)))))))))

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
    (when-let ((last (-last-item unwind-forms)))
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
         (condition-reachable (trinary-true)))
    (elsa-save-scope scope
      (-each body
        (lambda (arg)
          (elsa-with-reachability state condition-reachable
            (elsa--analyse-form arg scope state)
            (when (trinary-possible-p condition-reachable)
              (setq return-type
                    (elsa-type-sum return-type (oref arg type))))
            (elsa-scope-narrow-var scope (oref arg narrow-types)
                                   'elsa-variable-diff)
            (setq condition-reachable
                  (trinary-and
                   condition-reachable
                   (elsa-type-is-nil arg)))))))
    (-when-let (grouped (elsa-variables-group-and-sum
                         (->> body
                              (--filter (trinary-possible-p (elsa-form-reachable it)))
                              (--mapcat (oref it narrow-types))
                              (-non-nil))))
      (oset form narrow-types grouped))

    ;; If the last form is definitely never reachable, the form always
    ;; succeds and we make it non-nullable.
    (when (trinary-false-p condition-reachable)
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

    (let ((narrowed-vars
           ;; get a union of all narrowed variables
           (-uniq
            (--map
             (elsa-get-name it)
             (-non-nil
              (--mapcat (oref it narrow-types) body))))))
      ;; update narrowing of all the forms by the variables they don't
      ;; have assigned to their narrow-types
      (-each body
        (lambda (arg)
          (let ((nil-always-accepts (elsa-type-could-accept
                                     (elsa-type-nil)
                                     (elsa-get-type arg)))
                (narrow-types (oref arg narrow-types)))
            ;; add uncertainty to narrowing of all narrowed variables
            ;; if this form can fail.
            (when (trinary-maybe-p nil-always-accepts)
              (-each narrowed-vars
                (lambda (var-name)
                  (when (--none? (eq var-name (elsa-get-name it)) narrow-types)
                    (push (elsa-variable
                           :name var-name
                           ;; intersect with mixed will keep the
                           ;; original type, but the (un)certainty
                           ;; will propagate
                           :type (elsa-type-mixed)
                           :certainty (trinary-maybe))
                          narrow-types))))
              (oset arg narrow-types narrow-types))))))

    ;; Group all the narrowing variables by name and intersect the
    ;; narrowing types, because we require all forms to be
    ;; simultaneously true.  Set the resulting intersections on this
    ;; form.
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
     (body
      (setq return-type (oref (-last-item body) type))))

    ;; If the last form is not definitely reachable, it means we
    ;; could've failed somewhere inside the and form and we need to
    ;; make the return type nullable.
    (unless (trinary-necessary-p condition-reachable)
      (setq return-type (elsa-type-make-nullable return-type)))

    (oset form type return-type)))

(defun elsa--get-default-function-types (args)
  "Return a default list of types based on ARGS.

ARGS should be a list of `elsa-form's.

This function skips over special &optional and &rest markers and
collects all the arguments, turns &optional arguments into
nullables and the &rest argument into a variadic."
  (-let ((optionalp nil)
         (restp nil)
         (types nil))
    (elsa-form-foreach args
      (lambda (arg)
        (cond
         ((eq (elsa-get-name arg) '&optional)
          (setq optionalp t))
         ((eq (elsa-get-name arg) '&rest)
          (setq restp t)
          (setq optionalp nil))
         (t
          (let ((type (or (elsa-get-type arg)
                          (elsa-type-mixed))))
            (cond
             (optionalp (push (elsa-type-sum
                               (elsa-make-type nil)
                               type)
                              types))
             (restp (push (elsa-variadic-type
                           :item-type (elsa-type-sum
                                       (elsa-make-type nil)
                                       type))
                          types))
             (t (push type types))))))))
    (nreverse types)))

(defun elsa--analyse-defun-like-form (name args body form scope state)
  "Analyse function or macro definition.

A definition is a form like `defun', `cl-defun', `cl-defmethod'
and similar forms.

Care needs to be taken to not use the `form' argument to extract
structural information, such as function name or arglist, because
this function can be called from different contexts.  The
required information is instead passed as arguments.

This function tries to infer and validate the return type and the
argument types.

This function does not perform the call-site analysis, that is
handled by `elsa--analyse-function-like-invocation'."
  (let* (;; TODO: there should be an api for `(get name
         ;; 'elsa-type)'... probably on `scope', but for now scope is
         ;; separate for each processed file which is not great.
         (function-type (get name 'elsa-type))
         (vars))
    ;; Update the arg list if it was not already defined.  This
    ;; happens when the function was first registered with a reader
    ;; annotation.
    (-when-let (def (elsa-state-get-defun state name))
      (unless (slot-boundp def 'arglist)
        (oset def arglist (elsa-form-to-lisp args))))
    (when args
      (-each-indexed (--remove
                      (memq (elsa-get-name it) '(&rest &optional))
                      args)
        (lambda (index arg)
          (let ((var
                 (elsa-variable
                  :name (elsa-get-name arg)
                  :type (cond
                         ((not (elsa-type-mixed-p (elsa-get-type arg)))
                          (elsa-get-type arg))
                         ((not function-type)
                          (elsa-type-mixed))
                         (t
                          (let ((expected-type (elsa-function-type-nth-arg
                                                function-type
                                                index)))
                            (or expected-type
                                (progn
                                  (elsa-state-add-message state
                                    (elsa-make-warning arg
                                      "Argument no %d. `%s' is present in function declaration but is missing in Elsa type signature.  Use of top-level implicit mixed is discouraged."
                                      (1+ index)
                                      (elsa-get-name arg)))
                                  (elsa-type-mixed)))))))))
            (push var vars)
            (elsa-scope-add-var scope var)))))
    (when body (elsa--analyse-body body scope state))
    ;; check if return type of defun corresponds with the last form of
    ;; the body
    (let* ((body-return-type (if body (oref (-last-item body) type) (elsa-type-nil)))
           (function-return-type (elsa-type-get-return function-type)))
      (when  function-return-type
        (unless (elsa-type-assignable-p function-return-type body-return-type)
          (elsa-state-add-message state
            (elsa-make-error (elsa-car form)
              "Function is expected to return %s but returns %s."
              (elsa-type-describe function-return-type)
              (elsa-type-describe body-return-type)))))
      ;; infer the type of the function
      (unless (elsa-state-get-defun state name)
        (elsa-state-add-defun state
          (elsa-defun :name name
                      :type (elsa-function-type
                             :args (elsa--get-default-function-types args)
                             :return body-return-type)
                      :arglist (elsa-form-to-lisp args)))))
    (--each vars (elsa-scope-remove-var scope it))))

(defun elsa--analyse:defun (form scope state)
  (let* ((sequence (elsa-form-sequence form))
         (name (elsa-get-name (nth 1 sequence)))
         (args (elsa-form-sequence (nth 2 sequence)))
         (body (nthcdr 3 sequence)))
    (elsa--analyse-defun-like-form name args body form scope state)))

(defun elsa--analyse-register-functionlike-declaration (name args state)
  "Register functionlike declaration NAME with ARGS to STATE.

The registered object can be a `defun', `defmacro', or
`cl-defgeneric'."
  (cond
   ((elsa-state-get-defun state name)
    (-when-let (def (elsa-state-get-defun state name))
      (unless (slot-boundp def 'arglist)
        (oset def arglist (elsa-form-to-lisp args)))))
   (t
    (elsa-state-add-defun state
      (elsa-defun :name name
                  :type (elsa-function-type
                         :args (elsa--get-default-function-types args)
                         :return (elsa-type-mixed))
                  :arglist (elsa-form-to-lisp args))))))

(defun elsa--analyse:defmacro (form _scope state)
  "just skip for now, it's too complicated."
  (let ((name (elsa-get-name (elsa-cadr form)))
        (args (elsa-nth 2 form)))
    (elsa--analyse-register-functionlike-declaration name args state)))

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
         (def (elsa-state-get-defvar state var-name))
         (var-type (and def (elsa-get-type def))))
    (when value
      (elsa--analyse-form value scope state))
    (if var-type
        (unless (or (not value)
                    (elsa-type-assignable-p var-type (elsa-get-type value)))
          (elsa-state-add-message state
            (elsa-make-error value
              "Variable %s expects `%s', got `%s'"
              var-name
              (elsa-type-describe var-type)
              (elsa-type-describe (elsa-get-type value)))))
      (if value
          (elsa-state-add-defvar state
            (elsa-defvar :name var-name :type (oref value type)))
        (elsa-state-add-defvar state
          (elsa-defvar :name var-name :type (elsa-make-type unbound)))))))

(defun elsa--analyse:defvar-local (form scope state)
  (elsa--analyse:defvar form scope state))

(defun elsa--analyse:defcustom (form scope state)
  "Analyze `defcustom'.

The analysis works the same way as `elsa--analyse:defvar' except
we take the :type property of the defcustom into account when
automatically deriving the type."
  (let* ((name (elsa-nth 1 form))
         (value (elsa-nth 2 form))
         (var-name (elsa-get-name name))
         (def (elsa-state-get-defvar state var-name))
         (var-type (and def (elsa-get-type def))))
    (when value
      (elsa--analyse-form value scope state))
    (if var-type
        (unless (or (not value)
                    (elsa-type-assignable-p var-type (elsa-get-type value)))
          (elsa-state-add-message state
            (elsa-make-error value
              "Variable %s expects `%s', got `%s'"
              var-name
              (elsa-type-describe var-type)
              (elsa-type-describe (elsa-get-type value)))))
      ;; TODO: check the `:type' form here and also compare if we
      ;; are doing a valid assignment.
      (if value
          (elsa-state-add-defvar state
            (elsa-defvar :name var-name :type (oref value type)))
        (elsa-state-add-defvar state
          (elsa-defvar :name var-name :type (elsa-make-type unbound)))))))

(defun elsa--analyse:defconst (form scope state)
  "Analyze `defconst'.

If no type annotation is provided, find the value type through
`elsa--analyse:defvar' and wrap it as read-only."
  (elsa--analyse:defvar form scope state)
  (let* ((name (elsa-nth 1 form))
         (var-name (elsa-get-name name))
         (def (elsa-state-get-defvar state var-name))
         (var-type (and def (elsa-get-type def))))
    (when var-type
      (unless (elsa-readonly-type-p var-type)
        (oset def type (elsa-readonly-type :type var-type))
        ;; need to re-add to update the symbol property
        (elsa-state-add-defvar state def)))))

(defun elsa--analyse:defsubst (form scope state)
  (elsa--analyse:defun form scope state))

;; TODO: add support for reading type annotations for lambda forms
(defun elsa--analyse:lambda (form scope state)
  (let* ((sequence (oref form sequence))
         (args (nth 1 sequence))
         (body (nthcdr 2 sequence))
         ;; TODO: this should use `elsa--get-default-function-types'
         (arg-types (-repeat (length (elsa-form-sequence args))
                             (elsa-make-type mixed)))
         (vars))
    (when (elsa-form-list-p args)
      (-each-indexed (elsa-form-sequence args)
        (lambda (index arg)
          (let ((var (elsa-variable
                      :name (elsa-get-name arg)
                      :type (nth index arg-types))))
            (push var vars)
            (elsa-scope-add-var scope var)))))
    (when body
      (elsa--analyse-body body scope state))
    (--each vars (elsa-scope-remove-var scope it))
    (oset form type (elsa-function-type
                     :args arg-types
                     :return (if body
                                 (oref (-last-item body) type)
                               (elsa-make-type nil))))))

(defun elsa--analyse:function (form _scope _state)
  (let* ((arg (elsa-cadr form))
         (name (elsa-get-name arg))
         (type (elsa-function-get-type name)))
    (when (and type (elsa-form-symbol-p arg))
      (oset form type type))))

(defun elsa--analyse:quote (form _scope state)
  (oset state quoted (trinary-true))
  (let ((arg (cadr (oref form sequence))))
    (cond
     ((elsa-form-list-p arg)
      ;; TODO: make sure we analyze everything as static when quoted
      ;; (elsa--analyse-list arg scope state)
      (oset form type
            (elsa-type-list
             :item-type
             (-reduce-from
              (lambda (acc form)
                (elsa-type-sum acc (elsa-get-type form)))
              (elsa-type-empty)
              (elsa-form-sequence arg)))))
     ((elsa-form-symbol-p arg)
      (let* ((funciton-type (elsa-function-get-type (elsa-get-name arg)))
             (const-type (elsa-const-type
                          :type (elsa-type-symbol)
                          :value (elsa--quoted-symbol-name form)))
             (final-type (if funciton-type
                             (elsa-intersection-type
                              :types (list const-type funciton-type))
                           const-type)))
        (oset form type final-type)))
     ((elsa-form-keyword-p arg)
      (oset form type (elsa-type-keyword)))
     ((elsa-form-string-p arg)
      (oset form type (elsa-type-string)))
     ((elsa-form-integer-p arg)
      (oset form type (elsa-type-int)))))
  (oset state quoted (trinary-false)))

(defun elsa--analyse--validate-interactive-string (state arg)
  (let ((str (elsa-form-sequence arg))
        (allowed-codes "[abBcCdDefFGikKmMnNpPrsSUvxXzZ]")
        (case-fold-search nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (skip-chars-forward "@^*")
      (unless (eolp)
        (insert "\n")
        (backward-char 1)
        (while (search-forward "\n" nil t)
          (unless (or (looking-at-p allowed-codes)
                      (looking-at-p "^$"))
            (elsa-state-add-message state
              (elsa-make-error
                  arg
                "Unknown interactive code letter: %c"
                (char-after (point))))))))))

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

;; (elsa--analyse-normalize-spec :: (function ((or bool (list bool)) mixed) (list bool)))
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

(defun elsa--analyse-function-like-invocation (form spec head args scope state)
  "Analyse function or macro invocation.

A function invocation can be direct or indirect.

A direct invocation is simply a list form

    (function arg1 arg2 ...).

An indirect invocation is a function called through `funcall' or
`apply'.

FORM the form being analyzed.  This should be only used to
update its type or narrowing information.

SPEC determines which arguments are evaluated (used for macros).

HEAD is the form representing the function.  It can be the head
symbol of a list, a symbol used as a variable holding a function
or a quoted or function-quoted symbol.  It should be a form
object.

ARGS is a list of argument forms.

SCOPE and STATE are the scope and state objects."
  (setq spec (elsa--analyse-normalize-spec spec form))
  (let* ((name (elsa-get-name head))
         (type (elsa-get-type head))
         (narrow-type (elsa-function-get-narrow-type name)))
    (-each (-zip args spec)
      (-lambda ((arg . analysep))
        (when analysep
          (elsa--analyse-form arg scope state))))
    ;; check arity
    (when name
      (-let (((min . max) (elsa-fn-arity state name))
             (num-of-args (length args)))
        (when (eq max 'undefined)
          (elsa-state-add-message state
            (elsa-make-warning head
              "Function `%s' is missing arglist definition.  Maybe it is called before being declared?"
              name)))
        (when (< num-of-args min)
          (elsa-state-add-message state
            (elsa-make-error head
              "Function `%s' expects at least %d %s but received %d"
              name min
              (elsa-pluralize "argument" min)
              num-of-args)))
        (when (and (not (memq max '(many unevalled undefined)))
                   (> num-of-args max))
          (elsa-state-add-message state
            (elsa-make-error head
              "Function `%s' expects at most %d %s but received %d"
              name max
              (elsa-pluralize "argument" max)
              num-of-args)))))
    ;; check the types
    (when type
      ;; analyse the arguments
      (let* ((overloads-errors nil)
             (usable-overloads
              (catch 'no-overloads
                (when (elsa-type-callable-p type)
                  (-when-let* ((all-overloads (elsa-function-get-overloads type))
                               ;; overloads is progressively filtered
                               ;; to only the overloads applicable at
                               ;; current caller
                               (overloads (--map-indexed (cons it it-index) all-overloads)))
                    (-map-indexed
                     (lambda (index argument-form)
                       (let* ((good-overloads
                               ;; Check each argument against all
                               ;; possible overloads.  Keep only those
                               ;; which can match current arguments.
                               ;; If we run out of overloads, report
                               ;; the last set of possible overloads as
                               ;; the error.
                               (-filter
                                (-lambda ((overload . overload-index))
                                  (let* ((expected (elsa-function-type-nth-arg overload index))
                                         (expected-normalized
                                          (or expected (elsa-type-mixed)))
                                         (actual (oref argument-form type))
                                         (acceptablep
                                          (elsa-type-assignable-p
                                           expected-normalized
                                           actual)))
                                    (cond
                                     ((not expected)
                                      (push (list
                                             overload
                                             overload-index
                                             (format
                                              "Argument %d is present but the function signature does not define it.  Missing overload?"
                                              (1+ index)))
                                            overloads-errors))
                                     ((not acceptablep)
                                      (push (list
                                             overload
                                             overload-index
                                             ;; we need to sanitize
                                             ;; the % sign in the
                                             ;; error because it is
                                             ;; later passed to format
                                             ;; in `elsa-make-error'.
                                             ;; And the "but received"
                                             ;; string can contain
                                             ;; arbitrary text as one
                                             ;; possible type is
                                             ;; (const "whatever")
                                             (replace-regexp-in-string
                                              "%"
                                              "%%"
                                              (format
                                               "Argument %d accepts type `%s' but received `%s'"
                                               (1+ index)
                                               (elsa-type-describe expected-normalized)
                                               (elsa-type-describe actual))))
                                            overloads-errors)))
                                    (and expected acceptablep)))
                                overloads)))
                         (if good-overloads
                             ;; If we have multiple overloads where
                             ;; the argument is of a concrete type,
                             ;; that is not a sum or intersection
                             ;; (where the analysis is a bit more
                             ;; difficult for now), we can eliminate
                             ;; all the "less specific" overloads and
                             ;; only pick the one with the narrowest
                             ;; domain which still accepts the
                             ;; argument.  Because the
                             ;; `good-overloads' are only those which
                             ;; accept the arguments, we can sort them
                             ;; by `elsa-type-accept' and pick the
                             ;; last (smallest) one.
                             (if (elsa-type-composite-p (oref argument-form type))
                                 (setq overloads good-overloads)
                               (setq overloads
                                     (list
                                      (car
                                       (-sort
                                        (lambda (a b)
                                          (elsa-type-accept
                                           (elsa-function-type-nth-arg (car b) index)
                                           (elsa-function-type-nth-arg (car a) index)))
                                        good-overloads)))))
                           (elsa-state-add-message state
                             (if (< 1 (length overloads-errors))
                                 (elsa-make-error argument-form
                                   "No overload matches this call.\n%s"
                                   (s-join
                                    "\n"
                                    (-map
                                     (lambda (err)
                                       (format "  Overload %d of %d: '%s'\n    %s"
                                               (1+ (nth 1 err))
                                               (length all-overloads)
                                               (elsa-type-describe (car err))
                                               (nth 2 err)))
                                     (-sort (-on #'< #'cadr) overloads-errors))))
                               (elsa-make-error argument-form
                                 (nth 2 (car overloads-errors)))))
                           (throw 'no-overloads nil))))
                     args)
                    (mapcar #'car overloads))))))
        ;; set the return type of the form according to the return type
        ;; of the function's declaration
        (if usable-overloads
            (oset
             form type
             (elsa-type-sum-all (-map #'elsa-type-get-return usable-overloads)))
          (oset form type (elsa-type-get-return type)))))

    ;; compute narrowed types of variables in this function if possible
    (when narrow-type
      (-when-let (varname (elsa--analyse-arg-variable-p (car args)))
        (oset form narrow-types (list (elsa-variable :name varname :type narrow-type))))
      (-when-let (arg (car args))
        (let* ((arg-type (oref arg type))
               (could-accept (elsa-type-could-accept narrow-type arg-type)))
          (cond
           ((trinary-true-p could-accept)
            (oset form type (elsa-type-t)))
           ((trinary-false-p could-accept)
            (oset form type (elsa-type-nil)))))))))

;; (elsa--analyse-macro :: (function (mixed (or bool (or (list bool))) mixed mixed) mixed))
(defun elsa--analyse-macro (form spec scope state)
  (let* ((head (elsa-car form))
         (name (elsa-get-name head))
         (type (get name 'elsa-type))
         (args (elsa-cdr form)))
    (when type (oset head type type))
    (elsa--analyse-function-like-invocation
     form spec head args scope state)))

(defun elsa--analyse-function-call (form scope state)
  (elsa--analyse-macro form t scope state))

(defun elsa--analyse-list (form scope state)
  ;; handle special forms
  (let ((head (elsa-car form)))
    (if (and
         (not (elsa-state-quoted-p state))
         (elsa-form-symbol-p head))
        (let* ((name (oref head name))
               (analyse-fn-name (intern (concat "elsa--analyse:" (symbol-name name)))))
          (pcase name
            ((guard (functionp analyse-fn-name))
             (funcall analyse-fn-name form scope state))
            (`\` (elsa--analyse-backquote form scope state))
            (`\, (elsa--analyse-unquote form scope state))
            (`\,@ (elsa--analyse-splice form scope state))
            ;; function call
            (_ (elsa--analyse-function-call form scope state))))
      (elsa--analyse-body (elsa-form-sequence form) scope state))))

(defun elsa--analyse-improper-list (_form _scope _state)
  nil)

;; (elsa--analyse-form :: (function ((struct elsa-form) (struct elsa-scope) (struct elsa-state)) mixed))
(defun elsa--analyse-form (form scope state)
  "Analyse FORM.

FORM is a result of `elsa-read-form'."
  (oset form reachable (elsa-state-get-reachability state))
  (when-let ((annotation (oref form annotation)))
    (cond
     ((eq (car annotation) 'var)
      (let ((var (elsa-scope-get-var scope (cadr annotation))))
        ;; update the type in the current scope
        (oset var type (eval `(elsa-make-type ,@(nthcdr 3 annotation))))))))

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
      (elsa-check-check it form scope state)))
  (when-let ((method (oref state lsp-method))
             (params (oref state lsp-params)))
    (cond
     ((equal method "textDocument/hover")
      (-let* (((&HoverParams :position (&Position :line :character))
               params))
        (when (and (= (oref form line) (1+ line))
                   (<= (oref form column) character)
                   (or (< (1+ line) (oref form end-line))
                       (<= character (oref form end-column))))
          (throw 'lsp-response
                 (lsp-make-hover
                  :contents (lsp-make-markup-content
                             :kind "plaintext"
                             :value (format
                                     "%s: %s"
                                     (elsa-form-print form)
                                     (elsa-type-describe (elsa-get-type form))))
                  :range (lsp-make-range
                          :start (lsp-make-position
                                  :line (1- (oref form line))
                                  :character (oref form column))
                          :end (lsp-make-position
                                :line (1- (oref form end-line))
                                :character  (oref form end-column))))))))
     ((equal method "textDocument/completion")
      (-let* (((&CompletionParams :position (&Position :line :character))
               params))
        (when (and (= (oref form line) (1+ line))
                   (<= (oref form column) character)
                   (or (< (1+ line) (oref form end-line))
                       (<= character (oref form end-column))))
          (when form (message "form %s" (elsa-tostring form)))
          (when-let ((call-form (if (elsa-form-function-call-p form)
                                    form
                                  (and (slot-boundp form 'parent)
                                       (oref form parent)
                                       (elsa-form-function-call-p (oref form parent))
                                       (oref form parent)))))
            ;; special completion inside a function call form
            (message "call-form %s" (elsa-tostring call-form))
            (cond
             ((eq (elsa-get-name call-form) 'oref)
              (let* ((inst-form (elsa-cadr call-form))
                     (inst-type (elsa-get-type inst-form)))
                (when (elsa-struct-type-p inst-type)
                  (when-let* ((struct (get (oref inst-type name) 'elsa-cl-structure)))
                    (let ((slots (-mapcat
                                  (lambda (par)
                                    (when-let ((par-struct (get par 'elsa-cl-structure)))
                                      (oref par-struct slots)))
                                  (mapcar #'car (oref struct parents)))))
                      (throw 'lsp-response
                             (lsp-make-completion-list
                              :is-incomplete json-false
                              :items (elsa-lsp--list-completion-items
                                      slots 'symbol-name))))))))))

          ;; could still be a generic function call with point at the
          ;; first arg
          (when (or (elsa-form-function-call-p form)
                    (and (elsa-form-symbol-p form)
                         (eq (elsa-get-name form) 'nil)))
            (message "generic call form %s" (elsa-tostring form))
            (save-excursion
              (goto-char (1- (oref form end)))
              (when-let ((candidates (elsa-lsp--completions)))
                (throw 'lsp-response
                       (lsp-make-completion-list
                        :is-incomplete json-false
                        :items (elsa-lsp--list-completion-items candidates))))))

          ;; regular symbol, we should use defvars and scope
          ;; variables
          (message "variable form %s" (elsa-tostring form))
          (throw 'lsp-response
                 (lsp-make-completion-list
                  :is-incomplete json-false
                  :items (elsa-lsp--list-completion-items
                          (append (hash-table-keys (oref scope vars))
                                  (elsa-state-get-var-symbols state))
                          'symbol-name)))))))))

(defun elsa--analyse-body (body scope state)
  (--each body (elsa--analyse-form it scope state)))

(provide 'elsa-analyser)
