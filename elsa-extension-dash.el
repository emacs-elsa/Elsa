(require 'cl-macs)

(require 'elsa-reader)
(require 'elsa-analyser)
(require 'elsa-type-helpers)
(require 'elsa-state)
(require 'elsa-declare)

(put '!cons 'elsa-type (elsa-make-type (function (mixed mixed) (cons mixed mixed))))
;; The type really is Cons a b -> b but pragmatically this is used on lists
(put '!cdr 'elsa-type (elsa-make-type (function ((list mixed)) (list mixed))))
(put '-each 'elsa-type (elsa-make-type (function ((list mixed) (function (mixed) mixed)) mixed)))
(elsa-in-file "dash.el")
(elsa-declare-defun -each-indexed (list fn) (function ((list mixed) (function (int mixed) mixed)) mixed))
;; (put '-each-while 'elsa-type (elsa-make-type))
;; (put '-doto 'elsa-type (elsa-make-type))
;; (put '-dotimes 'elsa-type (elsa-make-type))
;; (put '-map 'elsa-type (elsa-make-type))
;; (put '-reduce-from 'elsa-type (elsa-make-type))
;; (put '-reduce 'elsa-type (elsa-make-type))
;; (put '-reduce-r-from 'elsa-type (elsa-make-type))
;; (put '-reduce-r 'elsa-type (elsa-make-type))
;; (put '-reductions-from 'elsa-type (elsa-make-type))
;; (put '-reductions-r-from 'elsa-type (elsa-make-type))
;; (put '-reductions 'elsa-type (elsa-make-type))
;; (put '-reductions-r 'elsa-type (elsa-make-type))
;; (put '-filter 'elsa-type (elsa-make-type))
;; (put '-select 'elsa-type (elsa-make-type))
;; (put '-remove 'elsa-type (elsa-make-type))
;; (put '-reject 'elsa-type (elsa-make-type))
;; (put '-remove-first 'elsa-type (elsa-make-type))
;; (put '-reject-first 'elsa-type (elsa-make-type))
;; (put '-remove-last 'elsa-type (elsa-make-type))
;; (put '-reject-last 'elsa-type (elsa-make-type))
;; (put '-remove-item 'elsa-type (elsa-make-type))
;; (put '-non-nil 'elsa-type (elsa-make-type))
;; (put '-keep 'elsa-type (elsa-make-type))
;; (put '-map-indexed 'elsa-type (elsa-make-type))
;; (put '-splice 'elsa-type (elsa-make-type))
;; (put '-splice-list 'elsa-type (elsa-make-type))
;; (put '-map-when 'elsa-type (elsa-make-type))
;; (put '-replace-where 'elsa-type (elsa-make-type))
;; (put '-map-first 'elsa-type (elsa-make-type))
;; (put '-map-last 'elsa-type (elsa-make-type))
;; (put '-replace 'elsa-type (elsa-make-type))
;; (put '-replace-first 'elsa-type (elsa-make-type))
;; (put '-replace-last 'elsa-type (elsa-make-type))
;; (put '-flatten 'elsa-type (elsa-make-type))
;; (put '-flatten-n 'elsa-type (elsa-make-type))
(put '-concat 'elsa-type (elsa-make-type (function (&rest (list mixed)) (list mixed))))
;; (put '-mapcat 'elsa-type (elsa-make-type))
;; (put '-copy 'elsa-type (elsa-make-type))
;; (put '-cons* 'elsa-type (elsa-make-type))
(put '-snoc 'elsa-type (elsa-make-type (function ((list mixed) mixed &rest mixed) (list mixed))))
;; (put '-first 'elsa-type (elsa-make-type))
;; (put '-find 'elsa-type (elsa-make-type))
;; (put '-some 'elsa-type (elsa-make-type))
;; (put '-any 'elsa-type (elsa-make-type))
;; (put '-last 'elsa-type (elsa-make-type))
;; (put '-first-item 'elsa-type (elsa-make-type))
;; (put '-second-item 'elsa-type (elsa-make-type))
;; (put '-third-item 'elsa-type (elsa-make-type))
;; (put '-fourth-item 'elsa-type (elsa-make-type))
;; (put '-fifth-item 'elsa-type (elsa-make-type))
;; (put '-last-item 'elsa-type (elsa-make-type))
;; (put '-butlast 'elsa-type (elsa-make-type))
;; (put '-count 'elsa-type (elsa-make-type))
(elsa-declare-defun -any? (pred list) (function ((function (mixed) mixed) (list mixed)) bool))
(elsa-declare-defun -any-p (pred list) (function ((function (mixed) mixed) (list mixed)) bool))
(elsa-declare-defun --any? (pred list) (function (mixed (list mixed)) bool))
(elsa-declare-defun --any-p (pred list) (function (mixed (list mixed)) bool))
;; (put '-some? 'elsa-type (elsa-make-type))
;; (put '-some-p 'elsa-type (elsa-make-type))
;; (put '-some-> 'elsa-type (elsa-make-type))
;; (put '-some->> 'elsa-type (elsa-make-type))
(elsa-declare-defun -all? (pred list) (function ((function (mixed) mixed) (list mixed)) bool))
(elsa-declare-defun -all-p (pred list) (function ((function (mixed) mixed) (list mixed)) bool))
;; (put '-every? 'elsa-type (elsa-make-type))
;; (put '-every-p 'elsa-type (elsa-make-type))
(elsa-declare-defun -none? (pred list) (function ((function (mixed) mixed) (list mixed)) bool))
(elsa-declare-defun -none-p (pred list) (function ((function (mixed) mixed) (list mixed)) bool))
;; (put '-only-some? 'elsa-type (elsa-make-type))
;; (put '-only-some-p 'elsa-type (elsa-make-type))
;; (put '-slice 'elsa-type (elsa-make-type))
;; (put '-take 'elsa-type (elsa-make-type))
;; (put '-drop 'elsa-type (elsa-make-type))
;; (put '-drop-last 'elsa-type (elsa-make-type))
;; (put '-take-last 'elsa-type (elsa-make-type))
;; (put '-take-while 'elsa-type (elsa-make-type))
;; (put '-drop-while 'elsa-type (elsa-make-type))
;; (put '-split-at 'elsa-type (elsa-make-type))
;; (put '-rotate 'elsa-type (elsa-make-type))
;; (put '-insert-at 'elsa-type (elsa-make-type))
;; (put '-replace-at 'elsa-type (elsa-make-type))
;; (put '-update-at 'elsa-type (elsa-make-type))
;; (put '-remove-at 'elsa-type (elsa-make-type))
;; (put '-remove-at-indices 'elsa-type (elsa-make-type))
;; (put '-split-with 'elsa-type (elsa-make-type))
;; (put '-split-on 'elsa-type (elsa-make-type))
;; (put '-split-when 'elsa-type (elsa-make-type))
;; (put '-separate 'elsa-type (elsa-make-type))
;; (put '-partition-all-in-steps 'elsa-type (elsa-make-type))
;; (put '-partition-in-steps 'elsa-type (elsa-make-type))
;; (put '-partition-all 'elsa-type (elsa-make-type))
;; (put '-partition 'elsa-type (elsa-make-type))
;; (put '-partition-after-item 'elsa-type (elsa-make-type))
;; (put '-partition-after-pred 'elsa-type (elsa-make-type))
;; (put '-partition-before-item 'elsa-type (elsa-make-type))
;; (put '-partition-before-pred 'elsa-type (elsa-make-type))
;; (put '-partition-by 'elsa-type (elsa-make-type))
;; (put '-partition-by-header 'elsa-type (elsa-make-type))
;; (put '-group-by 'elsa-type (elsa-make-type))
;; (put '-interpose 'elsa-type (elsa-make-type))
;; (put '-interleave 'elsa-type (elsa-make-type))
;; (put '-unzip 'elsa-type (elsa-make-type))
;; (put '-zip-with 'elsa-type (elsa-make-type))
;; (put '-zip 'elsa-type (elsa-make-type))
;; (put '-zip-fill 'elsa-type (elsa-make-type))
;; (put '-zip-pair 'elsa-type (elsa-make-type))
;; (put '-cycle 'elsa-type (elsa-make-type))
;; (put '-pad 'elsa-type (elsa-make-type))
;; (put '-annotate 'elsa-type (elsa-make-type))
;; (put '-table 'elsa-type (elsa-make-type))
;; (put '-table-flat 'elsa-type (elsa-make-type))
;; (put '-partial 'elsa-type (elsa-make-type))
;; (put '-elem-index 'elsa-type (elsa-make-type))
;; (put '-elem-indices 'elsa-type (elsa-make-type))
;; (put '-find-indices 'elsa-type (elsa-make-type))
;; (put '-find-index 'elsa-type (elsa-make-type))
;; (put '-find-last-index 'elsa-type (elsa-make-type))
;; (put '-select-by-indices 'elsa-type (elsa-make-type))
;; (put '-select-columns 'elsa-type (elsa-make-type))
;; (put '-select-column 'elsa-type (elsa-make-type))
;; (put '-grade-up 'elsa-type (elsa-make-type))
;; (put '-grade-down 'elsa-type (elsa-make-type))
;; (put '-> 'elsa-type (elsa-make-type))
;; (put '->> 'elsa-type (elsa-make-type))
;; (put '-as-> 'elsa-type (elsa-make-type))
;; (put '-when-let 'elsa-type (elsa-make-type))
;; (put '-when-let* 'elsa-type (elsa-make-type))
;; (put '-if-let 'elsa-type (elsa-make-type))
;; (put '-if-let* 'elsa-type (elsa-make-type))
;; (put '-let* 'elsa-type (elsa-make-type))
;; (put '-let 'elsa-type (elsa-make-type))
;; (put '-distinct 'elsa-type (elsa-make-type))
;; (put '-uniq 'elsa-type (elsa-make-type))
;; (put '-union 'elsa-type (elsa-make-type))
;; (put '-intersection 'elsa-type (elsa-make-type))
;; (put '-difference 'elsa-type (elsa-make-type))
;; (put '-powerset 'elsa-type (elsa-make-type))
;; (put '-permutations 'elsa-type (elsa-make-type))
;; (put '-inits 'elsa-type (elsa-make-type))
;; (put '-tails 'elsa-type (elsa-make-type))
;; (put '-common-prefix 'elsa-type (elsa-make-type))
;; (put '-contains? 'elsa-type (elsa-make-type))
;; (put '-contains-p 'elsa-type (elsa-make-type))
;; (put '-same-items? 'elsa-type (elsa-make-type))
;; (put '-same-items-p 'elsa-type (elsa-make-type))
;; (put '-is-prefix-p 'elsa-type (elsa-make-type))
;; (put '-is-prefix? 'elsa-type (elsa-make-type))
;; (put '-is-suffix-p 'elsa-type (elsa-make-type))
;; (put '-is-suffix? 'elsa-type (elsa-make-type))
;; (put '-is-infix-p 'elsa-type (elsa-make-type))
;; (put '-is-infix? 'elsa-type (elsa-make-type))
;; (put '-sort 'elsa-type (elsa-make-type))
;; (put '-list 'elsa-type (elsa-make-type))
(put '-repeat 'elsa-type (elsa-make-type (function (number mixed) (list mixed))))
;; (put '-sum 'elsa-type (elsa-make-type))
;; (put '-running-sum 'elsa-type (elsa-make-type))
;; (put '-product 'elsa-type (elsa-make-type))
;; (put '-running-product 'elsa-type (elsa-make-type))
;; (put '-max 'elsa-type (elsa-make-type))
;; (put '-min 'elsa-type (elsa-make-type))
;; (put '-max-by 'elsa-type (elsa-make-type))
;; (put '-min-by 'elsa-type (elsa-make-type))
;; (put '-iterate 'elsa-type (elsa-make-type))
;; (put '-fix 'elsa-type (elsa-make-type))
;; (put '-unfold 'elsa-type (elsa-make-type))
;; (put '-cons-pair? 'elsa-type (elsa-make-type))
;; (put '-cons-to-list 'elsa-type (elsa-make-type))
;; (put '-value-to-list 'elsa-type (elsa-make-type))
;; (put '-tree-mapreduce-from 'elsa-type (elsa-make-type))
;; (put '-tree-mapreduce 'elsa-type (elsa-make-type))
;; (put '-tree-map 'elsa-type (elsa-make-type))
;; (put '-tree-reduce-from 'elsa-type (elsa-make-type))
;; (put '-tree-reduce 'elsa-type (elsa-make-type))
;; (put '-tree-seq 'elsa-type (elsa-make-type))
;; (put '-tree-map-nodes 'elsa-type (elsa-make-type))
;; (put '-clone 'elsa-type (elsa-make-type))
;; (put '-rpartial 'elsa-type (elsa-make-type))
;; (put '-juxt 'elsa-type (elsa-make-type))
;; (put '-applify 'elsa-type (elsa-make-type))
;; (put '-on 'elsa-type (elsa-make-type))
;; (put '-flip 'elsa-type (elsa-make-type))
;; (put '-const 'elsa-type (elsa-make-type))
;; (put '-cut 'elsa-type (elsa-make-type))
;; (put '-orfn 'elsa-type (elsa-make-type))
;; (put '-andfn 'elsa-type (elsa-make-type))
;; (put '-iteratefn 'elsa-type (elsa-make-type))
;; (put '-fixfn 'elsa-type (elsa-make-type))
;; (put '-prodfn 'elsa-type (elsa-make-type))

(defun elsa-dash--update-anaphora-scope (scope)
  "Add the `it' variable to SCOPE."
  (let ((it-var (elsa-variable
                 :name 'it
                  ;; TODO: derive type based on the list argument type
                 :type (elsa-make-type mixed))))
    (elsa-scope-add-var scope it-var)
    it-var))

(defun elsa-dash--restore-anaphora-scope (it-var scope)
  "Remove the IT-VAR variable representing `it' from SCOPE."
  (elsa-scope-remove-var scope it-var))

(defun elsa-dash--analyse-anaphora (form scope state)
  (let ((it-var (elsa-dash--update-anaphora-scope scope)))
    (elsa--analyse-function-call form scope state)
    (elsa-dash--restore-anaphora-scope it-var scope)))

(defun elsa--analyse:-if-let (form scope state)
  (let* ((binding (elsa-cadr form))
         (condition (elsa-cadr binding))
         (true-body (elsa-nth 2 form))
         (false-body (elsa-nthcdr 3 form))
         (vars))
    (when binding
      (-when-let (vars-from-binding (elsa-dash--analyse-variable-from-binding binding scope state))
        (setq vars vars-from-binding)
        (--each vars-from-binding (elsa-scope-add-var scope it))))
    (elsa--analyse-form true-body scope state)
    (--each vars (elsa-scope-remove-var scope it))
    (elsa--analyse-body false-body scope state)
    (let ((true-result-type (elsa-get-type true-body))
          (false-result-type (if false-body
                                 (elsa-get-type (-last-item false-body))
                               (elsa-type-nil))))
      (oset form type (elsa-type-sum true-result-type false-result-type)))))

;; TODO: this does not follow the rules of `when' for reachability
;; propagation.  We really need to find some better way to unify these
;; kinds of macros because it will get unwieldy to copy the code
;; everywhere (also built-in `when-let' and possibly other control
;; macros)
(defun elsa--analyse:-when-let (form scope state)
  (let ((binding (cadr (oref form sequence)))
        (body (cddr (oref form sequence)))
        (vars))
    (when binding
      (-when-let (vars-from-binding (elsa-dash--analyse-variable-from-binding binding scope state))
        (setq vars vars-from-binding)
        (--each vars-from-binding (elsa-scope-add-var scope it))))
    (elsa--analyse-body body scope state)
    (--each vars (elsa-scope-remove-var scope it))
    (oset form type (elsa-get-type (-last-item body)))))

;; TODO: reachability
;; This is not always correct but at least we try to bind variables in
;; case the place is a simple symbol.  The logic is handled in
;; `elsa--analyse-variable-from-binding'
(defun elsa--analyse:-when-let* (form scope state)
  (let ((bindings (elsa-form-sequence (elsa-nth 1 form)))
        (body (cddr (oref form sequence)))
        (vars))
    (--each bindings
      (-when-let (vars-from-binding (elsa-dash--analyse-variable-from-binding it scope state))
        (--each vars-from-binding (elsa-scope-add-var scope it))
        (setq vars (-concat vars-from-binding vars))))
    (elsa--analyse-body body scope state)
    (--each vars (elsa-scope-remove-var scope it))
    (oset form type (elsa-get-type (-last-item body)))))

(defun elsa-dash--default-variable (name)
  (elsa-variable
   :name name
   ;; because we don't know how the
   ;; bindings relate to the source we
   ;; will default to mixed for now.
   :type (elsa-type-mixed)))

(defun elsa-dash--analyse-variable-from-binding (binding scope state)
  (-if-let (var (elsa--analyse-variable-from-binding binding scope state))
      (list var)
    ;; TODO: in case the standard analyser returned nil we might
    ;; be dealing with a dash matcher.  We will use a simple
    ;; heuristic to pick all the symbols from the matcher and
    ;; asssign a mixed type for now.  (message "%s"
    ;; (eieio-object-class it))
    (when (or (listp binding) (elsa-form-list-p binding))
      (-let [(pattern source) (elsa-form-sequence binding)]
        (when (or (cl-typep pattern 'elsa-form-seq)
                  (cl-typep pattern 'elsa-form-cons))
          (let (vars
                (maybe-autobind nil)
                (previous-form nil))
            (cl-flet ((create-autobind
                       ()
                       "The variable creation logic is extracted to flet binding to
                        reduce duplication."
                       (let* ((name (symbol-name
                                     (or (elsa--quoted-symbol-name previous-form)
                                         (elsa-get-name previous-form))))
                              (name-normalized (intern (replace-regexp-in-string "^:" "" name))))
                         (push (elsa-dash--default-variable name-normalized) vars))))
              (elsa-form-foreach pattern
                (lambda (form)
                  "Find all the symbol nodes in the variable declaration section."
                  (cond
                   ((and maybe-autobind
                         (or (elsa--quoted-symbol-p form)
                             (elsa-form-keyword-p form)))
                    (create-autobind))
                   ((or (elsa--quoted-symbol-p form)
                        (elsa-form-keyword-p form))
                    (setq maybe-autobind t))
                   ((and (elsa-form-symbol-p form)
                         (not (elsa-form-keyword-p form)))
                    (let ((name (elsa-get-name form)))
                      (when (not (string-match-p "^[&]" (symbol-name name)))
                        (push (elsa-dash--default-variable name) vars)))
                    (setq maybe-autobind nil)))
                  (setq previous-form form)))
              (when (and maybe-autobind
                         (or (elsa--quoted-symbol-p previous-form)
                             (elsa-form-keyword-p previous-form)))
                (create-autobind)))
            vars))))))

(defun elsa--analyse:-let (form scope state)
  (let ((bindings (let ((binding-form (elsa-cadr form)))
                    (cond
                     ((elsa-form-list-p binding-form)
                      (elsa-form-sequence binding-form))
                     ((elsa-form-vector-p binding-form)
                      (list (elsa-form-sequence binding-form))))))
        (body (elsa-nthcdr 2 form))
        (vars))
    (--each bindings
      (-when-let (vars-from-binding (elsa-dash--analyse-variable-from-binding it scope state))
        (setq vars (-concat vars-from-binding vars))))
    (--each vars (elsa-scope-add-var scope it))
    (if (not body)
        (oset form type (elsa-type-nil))
      (elsa--analyse-body body scope state)
      (oset form type (elsa-get-type (-last-item body))))
    (--each vars (elsa-scope-remove-var scope it))))

(defun elsa--analyse:-let* (form scope state)
  (let ((bindings (elsa-form-sequence (elsa-cadr form)))
        (body (elsa-nthcdr 2 form))
        (vars))
    (--each bindings
      (-when-let (vars-from-binding (elsa-dash--analyse-variable-from-binding it scope state))
        (--each vars-from-binding (elsa-scope-add-var scope it))
        (setq vars (-concat vars-from-binding vars))))
    (if (not body)
        (oset form type (elsa-type-nil))
      (elsa--analyse-body body scope state)
      (oset form type (elsa-get-type (-last-item body))))
    (--each vars (elsa-scope-remove-var scope it))))

(defun elsa--analyse:-lambda (form scope state)
  (let ((args (elsa-cadr form))
        (body (elsa-nthcdr 2 form))
        (vars nil))
    (elsa-form-foreach args
      (lambda (arg)
        (-when-let (vars-from-binding
                    (elsa-dash--analyse-variable-from-binding
                     (list arg) scope state))
          (setq vars (-concat vars-from-binding vars)))))
    (-each vars (lambda (v) (elsa-scope-add-var scope v)))
    (elsa--analyse-body body scope state)
    (-each vars (lambda (v) (elsa-scope-remove-var scope v)))
    (oset form type
          (elsa-function-type
           :args (list (elsa-make-type mixed))
           :return (clone (oref (-last-item body) type))))))

(defun elsa--analyse:->> (form scope state)
  (let ((initial (elsa-cadr form))
        (rest (elsa-nthcdr 2 form)))
    (elsa--analyse-form initial scope state)
    (elsa-form-foreach rest
      (lambda (f)
        (let* ((extended-form (clone f))
               (new-seq (copy-sequence (elsa-form-sequence extended-form))))
          (oset extended-form sequence (-snoc new-seq initial))
          (elsa--analyse-form extended-form scope state)
          ;;(message "type %s" (elsa-tostring (elsa-get-type extended-form)))
          (setq initial extended-form))))))

(defun elsa--analyse:--> (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--all-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--all? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--annotate (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--any (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--any-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--any? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--count (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--dotimes (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--drop-while (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--each (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--each-indexed (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--each-while (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--every-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--every? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--filter (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--find (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--find-index (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--find-indices (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--find-last-index (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--first (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--fix (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--group-by (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--if-let (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--iterate (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--keep (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--last (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map-first (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map-indexed (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map-last (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map-when (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--mapcat (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--max-by (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--min-by (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--none-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--none? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--only-some-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--only-some? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--partition-by (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--partition-by-header (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reduce (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reduce-from (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reduce-r (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reduce-r-from (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reject (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reject-first (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reject-last (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--remove (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--remove-first (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--remove-last (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--replace-where (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--select (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--separate (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--some (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--some-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--some? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--sort (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--splice (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--splice-list (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--split-when (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--split-with (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--take-while (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-map (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-map-nodes (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-mapreduce (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-mapreduce-from (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-reduce (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-reduce-from (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-seq (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--unfold (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--update-at (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--when-let (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--zip-with (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(provide 'elsa-extension-dash)
