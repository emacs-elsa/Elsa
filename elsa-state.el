(require 'eieio)

(require 'trinary)

(require 'elsa-scope)
(require 'elsa-error)

;; TODO: rename to elsa-structure
(defclass elsa-cl-structure nil
  ((name :initarg :name
         :documentation "Name of the structure.")
   (slots :initarg :slots
          :documentation "Slots available on the structure.

Does not include slots on parents.")
   (parents :type list
            :initarg :parents
            :documentation "Tree of parents of this structure."))
  :documentation "Representation of a `cl-defstruct' or `defclass'.")

(defclass elsa-defun nil
  ((name :initarg :name :documentation "Name of the defun.")
   (type :initarg :type :documentation "Function type of the defun.")
   (arglist :initarg :arglist :documentation "Defun arglist."))
  :documentation "Defun and defun-like definitions discovered during analysis.")

(defclass elsa-defvar nil
  ((name :initarg :name)
   (type :initarg :type)))

(defclass elsa-declarations nil
  ((defuns :type hash-table
           :initform (make-hash-table)
           :documentation "Defun-like declarations.")
   (cl-structures :type hash-table
                  :initform (make-hash-table)
                  :documentation "Structure-like declarations."))
  :documentation "Class holding discovered declarations.

Declarations are of various types:

- `elsa-defun' is any named callable object, such as `defun',
  `cl-defgeneric' or `defmacro'.
- `elsa-defvar' is any named variable, such as `defvar', `defconst' or
  `defcustom'.
- `elsa-cl-structure' is any named structure, such as `cl-defstruct'
  or EIEIO `defclass'.")

(cl-defgeneric elsa-state-add-structure ((this elsa-declarations) (str elsa-cl-structure))
  "Add `elsa-cl-structure' to state object."
  (declare (indent 1))
  (put (oref str name) 'elsa-cl-structure str)
  (puthash (oref str name) str (oref this cl-structures)))

(cl-defgeneric elsa-state-get-structure (this name)
  "Get from THIS the `elsa-cl-structure' definition called NAME.")

(cl-defgeneric elsa-state-add-defun ((this elsa-declarations) (def elsa-defun))
  "Add `elsa-defun' to state object."
  (declare (indent 1))
  (put (oref def name) 'elsa-type (oref def type))
  (puthash (oref def name) def (oref this defuns)))

(cl-defgeneric elsa-state-get-defun (this name)
  "Get from THIS the `elsa-defun' definition called NAME.")

(defclass elsa-global-state (elsa-declarations)
  ((project-directory :type string
                      :initarg :project-directory
                      :documentation "Directory from which the analysis was started.")
   (processed-file-index :type integer
                         :initarg :processed-file-index
                         :initform 1
                         :documentation "Index of currently processed file.")
   (number-of-files :type integer
                    :initarg :number-of-files
                    :documentation "Total number of files to analyze."))
  :documentation "Initial configuration and state of the entire analysis.

The other state class `elsa-state' state holds state of an analysis of
a single file, form or set of forms.")

(cl-defmethod elsa-state-get-defun ((this elsa-global-state) name)
  (gethash name (oref this defuns)))

(cl-defmethod elsa-state-get-structure ((this elsa-global-state) name)
  (gethash name (oref this cl-structures)))

(cl-defmethod elsa-global-state-get-counter ((this elsa-global-state))
  (let ((max-len (length (number-to-string (oref this number-of-files)))))
    (format (format "%%%dd/%%%dd" max-len max-len)
            (oref this processed-file-index)
            (oref this number-of-files))))

;; (elsa-global-state-prefix-length :: (function ((struct elsa-global-state) (or int nil)) int))
(cl-defmethod elsa-global-state-prefix-length ((this elsa-global-state) &optional extra)
  (let ((max-len (length (number-to-string (oref this number-of-files)))))
    (+ max-len 1 max-len (or extra 0))))

;; TODO: add some methods for looking up variables/defuns, so we don't
;; directly work with the hashtable
(defclass elsa-state (elsa-declarations)
  ((defvars :initform (make-hash-table))
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
   (scope :initform (elsa-scope))
   (global-state :type elsa-global-state
                 :initarg :global-state
                 :initform (elsa-global-state))))

(cl-defmethod elsa-state-update-global ((this elsa-state) (global elsa-global-state))
  "Copy declarations from THIS local state to GLOBAL state."
  (maphash (lambda (_name def)
             (elsa-state-add-defun global def))
           (oref this defuns))
  (maphash (lambda (_name def)
             (elsa-state-add-structure global def))
           (oref this cl-structures)))

(cl-defmethod elsa-state-get-defun ((this elsa-state) name)
  (or (gethash name (oref this defuns))
      (elsa-state-get-defun (oref this global-state) name)))

(cl-defmethod elsa-state-get-structure ((this elsa-state) name)
  (or (gethash name (oref this cl-structures))
      (elsa-state-get-structure (oref this global-state) name)))

(cl-defmethod elsa-state-ignore-line ((state elsa-state) line)
  (push line (oref state ignored-lines)))

;; TODO: take defvar directly? For consistency
(cl-defmethod elsa-state-add-defvar ((this elsa-state) name type)
  (put name 'elsa-type-var type)
  (let ((defvars (oref this defvars)))
    (puthash name (elsa-defvar :name name :type type) defvars)))

;; (elsa-state-add-message :: (function ((struct elsa-state) (struct elsa-message)) nil))
(defun elsa-state-add-message (state error)
  "In STATE, record an ERROR.

STATE is `elsa-state', ERROR is `elsa-message'."
  (declare (indent 1))
  (unless (memq (oref error line) (oref state ignored-lines))
    (push error (oref state errors))))


;; (elsa-state-get-reachability :: (function ((struct elsa-state)) (struct trinary)))
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

;; (elsa-state-quoted-p :: (function ((struct elsa-state)) bool))
(defun elsa-state-quoted-p (state)
  "Return non-nil if the current form is quoted."
  (trinary-true-p (oref state quoted)))

(provide 'elsa-state)
