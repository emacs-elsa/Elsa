;; -*- lexical-binding: t -*-

(require 'elsa-type-helpers)
(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-variable)
(require 'elsa-error)

(require 'eieio)
(require 'dash)

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
     ((get var 'elsa-type-var))
     (t (elsa-make-type Unbound)))))

(defun elsa--infer-symbol (form scope)
  (cond
   ((eq (oref form name) t) (elsa-make-type T))
   ((eq (oref form name) nil) (elsa-make-type Nil))
   (t (elsa--get-var-type (oref form name) scope :--placeholder--))))

(defun elsa--infer-unary-fn (form handler)
  (declare (indent 1))
  (-when-let (arg (cadr (oref form sequence)))
    (let ((arg1-type (oref arg type)))
      (funcall handler arg1-type))))

(provide 'elsa-infer)
