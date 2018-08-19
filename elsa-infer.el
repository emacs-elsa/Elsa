;; -*- lexical-binding: t -*-

(require 'elsa-type-helpers)
(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-variable)
(require 'elsa-error)

(require 'eieio)
(require 'dash)

(defun elsa--infer-unary-fn (form handler)
  (declare (indent 1))
  (-when-let (arg (cadr (oref form sequence)))
    (let ((arg1-type (oref arg type)))
      (funcall handler arg1-type))))

(provide 'elsa-infer)
