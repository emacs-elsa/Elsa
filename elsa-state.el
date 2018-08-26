(require 'trinary)

(require 'elsa-scope)

;; TODO: add some methods for looking up variables/defuns, so we don't
;; directly work with the hashtable
(defclass elsa-state nil
  ((defvars :initform (make-hash-table))
   (errors :initform nil)
   (reachable :initform (list (trinary-true)))
   (scope :initform (elsa-scope))))

;; TODO: take defvar directly? For consistency
(cl-defmethod elsa-state-add-defvar ((this elsa-state) name type)
  (let ((defvars (oref this defvars)))
    (puthash name (elsa-defvar :name name :type type) defvars)))

(cl-defmethod elsa-state-add-error ((this elsa-state) error)
  (oset this errors (cons error (oref this errors))))

(defun elsa-state-get-reachability (state)
  (car (oref state reachable)))

(defmacro elsa-with-reachability (state reachability &rest body)
  (declare (indent 2))
  `(progn
     (push ,reachability (oref ,state reachable))
     ,@body
     (pop (oref ,state reachable))))

(put 'elsa-state-add-error 'lisp-indent-function 1)

(provide 'elsa-state)
