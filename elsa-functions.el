;; -*- lexical-binding: t -*-

(provide 'elsa-functions)

;; (elsa-function-get-narrow-types :: (function (symbol) (list mixed)))
(defun elsa-function-get-narrow-types (name)
  "Get position narrow types of function NAME."
  (get name 'elsa-narrow-types))

;; (elsa-function-get-type :: (function (symbol) mixed))
(defun elsa-function-get-type (name)
  "Get type of the function NAME."
  (get name 'elsa-type))

(cl-defgeneric elsa-function-get-overloads (callable)
  "Get all overloads of CALLABLE.

Return a list of all overloads."
  nil)

(cl-defmethod elsa-function-get-overloads ((callable elsa-function-type))
  (list callable))

(cl-defmethod elsa-function-get-overloads ((callable elsa-sum-type))
  (oref callable types))

(cl-defmethod elsa-function-get-overloads ((callable elsa-intersection-type))
  (-filter #'elsa-type-callable-p (oref callable types)))
