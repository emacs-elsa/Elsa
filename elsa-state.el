(require 'elsa-scope)

;; TODO: add some methods for looking up variables/defuns, so we don't
;; directly work with the hashtable
(defclass elsa-state nil
  ((defvars :initform (make-hash-table))
   (defuns :initform (make-hash-table))
   (errors :initform nil)
   (scope :initform (elsa-scope))))

;; TODO: take defvar directly? For consistency
(defmethod elsa-state-add-defvar ((this elsa-state) name type)
  (let ((defvars (oref this defvars)))
    (puthash name (elsa-defvar :name name :type type) defvars)))

(defmethod elsa-state-add-defun ((this elsa-state) defun)
  (unless (elsa-defun-p defun) (error "defun must be `elsa-defun-p'"))
  (let ((defuns (oref this defuns)))
    (puthash (oref defun name) defun defuns)))

(defmethod elsa-state-add-error ((this elsa-state) error)
  (oset this errors (cons error (oref this errors))))

(put 'elsa-state-add-error 'lisp-indent-function 1)

(provide 'elsa-state)
