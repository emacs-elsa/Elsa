;; -*- lexical-binding: t -*-

(require 'elsa-type-helpers)
(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-variable)
(require 'elsa-error)

(require 'eieio)
(require 'dash)

(defvar elsa-infer-errors nil)

(defclass elsa-expression nil
  ((type :type elsa-type :initarg :type)
   (form :type elsa-form :initarg :form))
  :abstract t)

(defclass elsa-expression-atom (elsa-expression) ())
(defclass elsa-expression-var (elsa-expression) ())
(defclass elsa-expression-quote (elsa-expression) ())

(defclass elsa-expression-app (elsa-expression)
  ((arguments :initarg :arguments)))

(defclass elsa-expression-let (elsa-expression)
  ((bindings :initarg :bindings)
   (body :initarg :body)))

(defclass elsa-expression-if (elsa-expression)
  ((condition :initarg :condition)
   (then :initarg :then)
   (else :initarg :else)))

(defun elsa--get-var-type (var scope decls)
  (let (
        ;; TODO: ignores lexical binding
        ;; (declaration (elsa-scope-get-var decls var))
        (current (elsa-scope-get-var scope var)))
    (cond
     ;; ((and current declaration)
     ;;  (clone (oref declaration type)))
     (current
      (clone (oref current type)))
     (t (elsa-make-type 'unbound)))))

(defun elsa--narrow-type (condition)
  "Narrow types of expressions contained in CONDITION."
  (let ((pos nil)
        (neg nil))
    (pcase (eieio-object-class condition)
      (`elsa-expression-app
       (pcase (oref (oref (oref condition form) name) name)
         (`numberp
          (-let* ((type (oref (car (oref condition arguments)) type))
                  (name (oref (oref (car (oref condition arguments)) form) name))
                  ((positive-type . negative-type)
                   (if (eq (eieio-object-class type) 'elsa-type-unbound)
                       (cons type type)
                     (unless (elsa-type-accept type 'number)
                       (push (elsa-warning
                              :form condition
                              :message "Testing non-number with `numberp'.")
                             elsa-infer-errors))
                     (cons (elsa-make-type 'number)
                           (elsa-diff-type
                            :positive type
                            :negative (elsa-make-type 'number))))))
            (push (elsa-variable :name name :type positive-type) pos)
            (push (elsa-variable :name name :type negative-type) neg)))))
      (`elsa-expression-var
       (push (elsa-variable :name (oref (oref condition form) name)
                            :type (oref condition type)) pos)
       (push (elsa-variable :name (oref (oref condition form) name)
                            :type (elsa-make-type 'nil)) neg)))
    (cons pos neg)))

(defun elsa--infer-symbol (form scope)
  (cond
   ((eq (oref form name) t) (elsa-make-type 't))
   ((eq (oref form name) nil) (elsa-make-type 'nil))
   (t (elsa--get-var-type (oref form name) scope :--placeholder--))))

(defun elsa--infer-let (form scope decls)
  (let ((new-vars nil)
        (binding-exprs nil))
    (-each (oref form bindings)
      (lambda (binding)
        (pcase binding
          (`(,var ,form)
           (let ((binding-expr (elsa--infer-form form scope decls)))
             (push (elsa-variable
                    :name (oref var name) :type (oref binding-expr type))
                   new-vars)
             (push binding-expr binding-exprs)))
          (var
           (push (elsa-variable :name (oref var name) :type (elsa-make-type nil))
                 new-vars)))))
    (-each new-vars (lambda (v) (elsa-scope-add-variable scope v)))
    (let ((body
           (-map (lambda (x) (elsa--infer-form x scope decls))
                 (oref form body))))
      (prog1 (elsa-expression-let
              :type (oref (-last-item body) type)
              :form form
              :body body
              :bindings (nreverse binding-exprs))
        (-each new-vars (lambda (v) (elsa-scope-remove-variable scope v)))))))

(defun elsa--infer-let* (form scope decls)
  (let ((new-vars nil)
        (binding-exprs nil))
    (-each (oref form bindings)
      (lambda (binding)
        (let ((variable
               (pcase binding
                 (`(,var ,form)
                  (let ((binding-expr (elsa--infer-form form scope decls)))
                    (push binding-expr  binding-exprs)
                    (elsa-variable :name (oref var name)
                                   :type (oref binding-expr type))))
                 (var
                  (elsa-variable :name (oref var name) :type (elsa-make-type nil))))))
          (elsa-scope-add-variable scope variable)
          (push variable new-vars))))
    (let ((body
           (-map (lambda (x) (elsa--infer-form x scope decls))
                 (oref form body))))
      (prog1 (elsa-expression-let
              :type (oref (-last-item body) type)
              :form form
              :body body
              :bindings (nreverse binding-exprs))
        (-each new-vars (lambda (v) (elsa-scope-remove-variable scope v)))))))

(defun elsa--infer-if (form scope decls)
  (-let* ((cond-expr (elsa--infer-form (oref form condition) scope decls))
          ((pos . neg) (elsa--narrow-type cond-expr))
          (then-expr
           (prog2
               (-each pos (lambda (v) (elsa-scope-add-variable scope v)))
               (elsa--infer-form (oref form then) scope decls)
             (-each pos (lambda (v) (elsa-scope-remove-variable scope v)))))
          (else-expr
           (prog2
               (-each neg (lambda (v) (elsa-scope-add-variable scope v)))
               (-map (lambda (x) (elsa--infer-form x scope decls))
                     (oref form else))
             (-each neg (lambda (v) (elsa-scope-remove-variable scope v))))))
    (elsa-expression-if
     :form form
     :type (elsa-type-sum (oref then-expr type)
                          (oref (-last-item else-expr) type))
     :condition cond-expr
     :then then-expr
     :else else-expr)))

(defun elsa--infer-form (form scope decls)
  (pcase (eieio-object-class form)
    (`elsa-form-symbol
     (cond
      ((eq (oref form name) t)
       (elsa-expression-atom :type (elsa-make-type 't) :form form))
      ((eq (oref form name) nil)
       (elsa-expression-atom :type (elsa-make-type 'nil) :form form))
      (t
       (elsa-expression-var
        :type (elsa--get-var-type (oref form name) scope decls)
        :form form))))
    (`elsa-form-progn
     (let ((arguments (-map (lambda (x) (elsa--infer-form x scope decls))
                            (oref form body))))
       (elsa-expression-app
        :type (oref (-last-item arguments) type)
        :form form
        :arguments arguments)))
    (`elsa-form-let (elsa--infer-let form scope decls))
    (`elsa-form-let* (elsa--infer-let* form scope decls))
    (`elsa-form-if (elsa--infer-if form scope decls))
    (`elsa-form-function-call
     (elsa-expression-app
      :type (elsa-make-type nil)
      :form form
      :arguments (-map (lambda (x) (elsa--infer-form x scope decls))
                       (oref form args))))))

(defun elsa-infer-types (form scope decls)
  (setq elsa-infer-errors nil)
  (prog1 (elsa--infer-form form scope decls)
    (setq elsa-infer-errors (nreverse elsa-infer-errors))))

(provide 'elsa-infer)
