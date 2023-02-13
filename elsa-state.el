(require 'trinary)

(require 'elsa-scope)

;; TODO: add some methods for looking up variables/defuns, so we don't
;; directly work with the hashtable
(defclass elsa-state nil
  ((defuns :initform nil)
   (defvars :initform (make-hash-table))
   (errors :initform nil)
   (provide :initform nil
            :documentation "Symbol provided by current file")
   (requires :initform nil :type list
             :documentation "Symbols required by current file")
   (ignored-lines :initform nil)
   (reachable :initform (list (trinary-true)))
   ;; TODO: I don't remember or understand what this is.  I'm going to
   ;; commit it since I can't see it making any difference.  But it
   ;; probably serves some purpose.
   (quoted :initform (trinary-false))
   (scope :initform (elsa-scope))))

(defun elsa-state-add-defun (state name type)
  (put name 'elsa-type type)
  (push `(defun ,name ,type) (oref state defuns)))

(defun elsa-state-ignore-line (state line)
  (push line (oref state ignored-lines)))

;; TODO: take defvar directly? For consistency
(cl-defmethod elsa-state-add-defvar ((this elsa-state) name type)
  (put name 'elsa-type-var type)
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
  "Return the current reachability.

It is a trinary value of yes, no or maybe specifying whether the
currently analysed form is surely reachable, surely unreachable
or maybe reachable."
  (car (oref state reachable)))

(defmacro elsa-with-reachability (state reachability &rest body)
  "Set REACHABILITY of all expressions analysed in BODY.

After BODY is executed, restore previous reachability.

Reachability is tracked on the STATE."
  (declare (indent 2)
           (debug (form form body)))
  `(progn
     (push ,reachability (oref ,state reachable))
     ,@body
     (pop (oref ,state reachable))))

(defun elsa-state-quoted-p (state)
  (trinary-true-p (oref state quoted)))

(provide 'elsa-state)
