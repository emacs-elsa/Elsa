;; -*- lexical-binding: t -*-

(provide 'elsa-functions)

;; (elsa-function-get-narrow-types :: (function (symbol) (list mixed)))
(defun elsa-function-get-narrow-types (name)
  "Get position narrow types of function NAME."
  (get name 'elsa-narrow-types))

(cl-defgeneric elsa-function-get-overloads (callable)
  "Get all overloads of CALLABLE.

Return a list of all overloads."
  nil)

(cl-defmethod elsa-function-get-overloads ((callable elsa-function-type))
  (list callable))

(cl-defmethod elsa-function-get-overloads ((callable elsa-sum-type))
  (oref callable types))
