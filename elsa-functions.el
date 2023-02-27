;; -*- lexical-binding: t -*-

(require 'elsa-types)

;; (elsa-function-get-narrow-type :: (function (symbol) (list mixed)))
(defun elsa-function-get-narrow-type (name)
  "Get position narrow types of function NAME."
  (let* ((type (elsa-function-get-type name))
         (return-type (elsa-type-get-return type)))
    (when (elsa-type-is-type-predicate-p return-type)
      (oref return-type predicates))))

;; (elsa-function-get-type :: (function (symbol) mixed))
(defun elsa-function-get-type (name)
  "Get type of the function NAME."
  (get name 'elsa-type))

(cl-defgeneric elsa-function-get-overloads (_callable)
  "Get all overloads of CALLABLE.

Return a list of all overloads."
  nil)

(cl-defmethod elsa-function-get-overloads ((callable elsa-function-type))
  (list callable))

(cl-defmethod elsa-function-get-overloads ((callable elsa-sum-type))
  (oref callable types))

(cl-defmethod elsa-function-get-overloads ((callable elsa-intersection-type))
  (-filter #'elsa-type-callable-p (oref callable types)))

(provide 'elsa-functions)
