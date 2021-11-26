;; -*- lexical-binding: t -*-

(provide 'elsa-functions)

;; (elsa-function-get-narrow-types :: (function (symbol) (list mixed)))
(defun elsa-function-get-narrow-types (name)
  "Get position narrow types of function NAME."
  (get name 'elsa-narrow-types))

(defun elsa-callable-with-args-p (fun args)
  "Check if function FUN is callable with ARGS.

ARGS is a list of forms which represent arguments to function."
  (let ((arg-fun (elsa-function-type
                  :args (-map #'elsa-get-type args)
                  :return (elsa-type-mixed))))
    (elsa-type-accept arg-fun fun)))

(cl-defgeneric elsa-function-filter-overloads (callable args)
  "Filter overloads of CALLABLE by real ARGS.

CALLABLE is a callable type as determined by
`elsa-type-callable-p'.

ARGS is a list of `elsa-form' objects with assigned types.

An overload can be used if it can be called with all supplied
args.")

(cl-defmethod elsa-function-filter-overloads ((callable elsa-function-type) args)
  (elsa-callable-with-args-p callable args))

(cl-defmethod elsa-function-filter-overloads ((callable elsa-sum-type) args)
  (let ((types (oref callable types)))
    (elsa-sum-type :types (--filter (elsa-callable-with-args-p it args) types))))

(cl-defgeneric elsa-function-get-overloads (callable)
  "Get all overloads of CALLABLE.

Return a list of all overloads."
  nil)

(cl-defmethod elsa-function-get-overloads ((callable elsa-function-type))
  (list callable))

(cl-defmethod elsa-function-get-overloads ((callable elsa-sum-type))
  (oref callable types))
