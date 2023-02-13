(require 'trinary)

(require 'elsa-scope)

;; TODO: add some methods for looking up variables/defuns, so we don't
;; directly work with the hashtable
(defclass elsa-state nil
  ((defuns :initform (make-hash-table))
   (global-defuns :initform (make-hash-table)
                  :documentation "Already processed defuns from other files.")
   (defvars :initform (make-hash-table))
   (errors :initform nil)
   (provide :initform nil
            :documentation "Symbols provided by current file")
   (requires :initform nil :type list
             :documentation "Symbols required by current file")
   (ignored-lines :initform nil)
   (reachable :initform (list (trinary-true)))
   ;; TODO: I don't remember or understand what this is.  I'm going to
   ;; commit it since I can't see it making any difference.  But it
   ;; probably serves some purpose.
   (quoted :initform (trinary-false))
   (scope :initform (elsa-scope))))

(defclass elsa-defun nil
  ((name :initarg :name :documentation "Name of the defun.")
   (type :initarg :type :documentation "Function type of the defun.")
   (arglist :initarg :arglist :documentation "Defun arglist."))
  :documentation "Defun and defun-like definitions discovered during analysis.")

(cl-defgeneric elsa-state-add-defun (this def)
  (declare (indent 1)))

(cl-defmethod elsa-state-add-defun ((this elsa-state) (def elsa-defun))
  (put (oref def name) 'elsa-type (oref def type))
  (puthash (oref def name) def (oref this defuns)))

(cl-defgeneric elsa-state-get-defun ((this elsa-state) name)
  (or (gethash name (oref this defuns))
      (gethash name (oref this global-defuns))))

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
