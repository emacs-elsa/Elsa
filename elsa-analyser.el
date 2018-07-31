;; -*- lexical-binding: t -*-

(require 'elsa-reader)
(require 'elsa-rules-list)
(require 'elsa-check)
(require 'elsa-infer)
(require 'elsa-error)
(require 'elsa-types)

(require 'elsa-typed-builtin)

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
          (bindings (oref (cadr (oref form sequence)) sequence))
          (body (cddr (oref form sequence))))
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

(defun elsa--analyse-if (form scope)
  (let ((condition (nth 1 (oref form sequence)))
        (true-body (nth 2 (oref form sequence)))
        (false-body (nth 3 (oref form sequence))))
    (elsa--analyse-form condition scope)
    (elsa--analyse-form true-body scope)
    (when false-body (elsa--analyse-form false-body scope))))

(defun elsa--analyse-function-call (form scope)
  (let* ((errors)
         (head (elsa-form-car form))
         (name (oref head name))
         (args (cdr (oref form sequence)))
         (type (get name 'elsa-type)))
    (push (--map (elsa--analyse-form it scope) args) errors)
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

    (pcase name
      (`not
       (let ((arg-type (oref (car args) type)))
         (cond
          ((elsa-type-accept (elsa-type-nil) arg-type) ;; definitely false
           (oset form type (elsa-type-t)))
          ((not (elsa-type-accept arg-type (elsa-type-nil))) ;; definitely true
           (oset form type (elsa-type-nil)))
          (t (oset form type (elsa-make-type 't?))))))
      (`car
       (let ((arg (oref (car args) type)))
         (when (elsa-type-list-p arg)
           (oset form type (elsa-type-make-nullable (oref arg item-type))))))
      (`stringp
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
                   (elsa-make-type 't?))
                  (t (elsa-type-nil))))))))
    (-flatten errors)))

(defun elsa--analyse-list (form scope)
  ;; handle special forms
  (let ((head (elsa-form-car form)))
    (when (elsa-form-symbol-p head)
      (let ((name (oref head name)))
        (pcase name
          (`let (elsa--analyse-let form scope))
          (`if (elsa--analyse-if form scope))
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
    (--map (when (elsa-check-should-run it form)
             (elsa-check-check it form))
           elsa-checks))))

(provide 'elsa-analyser)
