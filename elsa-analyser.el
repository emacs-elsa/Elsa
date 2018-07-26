;; -*- lexical-binding: t -*-

(require 'elsa-reader)
(require 'elsa-rules-list)
(require 'elsa-check)
(require 'elsa-infer)
(require 'elsa-error)
(require 'elsa-types)

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
              (push (elsa--analyse-form source scope) errors)
              (push (elsa-variable
                     :name (oref var name) :type (oref source type))
                    new-vars)))
           ((elsa-form-symbol-p binding)
            (push (elsa-variable :name (oref binding name) :type (elsa-make-type nil))
                  new-vars)))))
      (-each new-vars (lambda (v) (elsa-scope-add-variable scope v)))
      (push (--map (elsa--analyse-form it scope) body) errors)
      (oset form type (oref (-last-item body) type))
      (-each new-vars (lambda (v) (elsa-scope-remove-variable scope v))))
    (-flatten errors)))

(defun elsa--analyse-if (form scope)
  (let (errors)
    (let ((condition (nth 1 (oref form sequence)))
          (true-body (nth 2 (oref form sequence)))
          (false-body (nth 3 (oref form sequence))))
      (elsa--analyse-form condition scope)
      ;; TODO: This is not analysis but a rule, move it to the checks
      (if (not (elsa-type-accept (oref condition type) (elsa-type-nil)))
          (push (elsa-warning
                 :expression condition
                 :message "Else clause will never be executed.")
                errors)
        (when (elsa-type-accept (elsa-type-nil) (oref condition type))
          (push (elsa-warning
                 :expression condition
                 :message "Then clause will never be executed.")
                errors))))
    (-flatten errors)))

(defun elsa--analyse-list (form scope)
  ;; handle special forms
  (let ((head (elsa-form-car form)))
    (when (elsa-form-symbol-p head)
      (let ((name (oref head name)))
        (pcase name
          (`let (elsa--analyse-let form scope))
          (`if (elsa--analyse-if form scope))))))
  ;; (-non-nil (-map 'elsa--analyse-form (oref form sequence)))
  )

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
    (--map (when (funcall (oref it predicate) form)
             (funcall (oref it check) form))
           elsa-checks))))
