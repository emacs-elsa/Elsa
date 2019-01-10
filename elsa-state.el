(require 'trinary)

(require 'elsa-scope)

;; TODO: add some methods for looking up variables/defuns, so we don't
;; directly work with the hashtable
(defclass elsa-state nil
  ((defuns :initform nil)
   (defvars :initform (make-hash-table))
   (errors :initform nil)
   (ignored-lines :initform nil)
   (reachable :initform (list (trinary-true)))
   (scope :initform (elsa-scope))))

(defun elsa-state-add-defun (state name type)
  (put name 'elsa-type type)
  (push `(defun ,name ,type) (oref state defuns)))

(defun elsa-state-ignore-line (state line)
  (push line (oref state ignored-lines)))

;; TODO: take defvar directly? For consistency
(cl-defmethod elsa-state-add-defvar ((this elsa-state) name type)
  (let ((defvars (oref this defvars)))
    (puthash name (elsa-defvar :name name :type type) defvars)))
(declare-function elsa-defvar "elsa" (&rest slots))

(defun elsa-state-add-message (state error)
  "In STATE, record an ERROR.

STATE is `elsa-state', ERROR is `elsa-message'."
  (declare (indent 1))
  (unless (memq (oref error line) (oref state ignored-lines))
    (push error (oref state errors))))


(defun elsa-state-get-reachability (state)
  (car (oref state reachable)))

(defmacro elsa-with-reachability (state reachability &rest body)
  (declare (indent 2))
  `(progn
     (push ,reachability (oref ,state reachable))
     ,@body
     (pop (oref ,state reachable))))

(provide 'elsa-state)
