(require 'eieio)

(require 'trinary)

(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-error)
(require 'elsa-methods)

;; TODO: rename to elsa-structure
(defclass elsa-cl-structure nil
  ((name :initarg :name
         :documentation "Name of the structure.")
   (slots :initarg :slots
          :documentation "Slots available on the structure.

Does not include slots on parents.")
   (parents :type list
            :initarg :parents
            :documentation "Tree of parents of this structure.")
   (file :initarg :file :documentation "File where the structure was declared."))
  :documentation "Representation of a `cl-defstruct' or `defclass'.")

(defclass elsa-defun nil
  ((name :initarg :name :documentation "Name of the defun.")
   (type :initarg :type :documentation "Function type of the defun.")
   (arglist :initarg :arglist :documentation "Defun arglist.")
   (file :initarg :file :documentation "File where the defun was declared."))
  :documentation "Defun and defun-like definitions discovered during analysis.")

(cl-defmethod elsa-get-type ((this elsa-defun))
  (oref this type))

(defclass elsa-defvar nil
  ((name
    :type symbol
    :initarg :name)
   (type
    :type elsa-type
    :initarg :type)
   (file
    :type (or string null)
    :initarg :file)))

(cl-defmethod elsa-get-type ((this elsa-defvar))
  (oref this type))

(defclass elsa-declarations nil
  ((defuns
     :type hash-table
     :initform (make-hash-table)
     :documentation "Defun-like declarations.")
   (defvars
     :type hash-table
     :initform (make-hash-table)
     :documentation "Defvar-like declarations.")
   (cl-structures
    :type hash-table
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
  (oset str file (elsa-state-current-file this))
  (put (oref str name) 'elsa-cl-structure str)
  (puthash (oref str name) str (oref this cl-structures)))

(cl-defgeneric elsa-state-clear-file-structures ((this elsa-declarations) file)
  "Remove from THIS state all structures declared in FILE."
  (let ((keys-to-remove nil)
        (cl-structures (oref this cl-structures)))
    (maphash
     (lambda (key value)
       (when (equal (oref value file) file)
         (push key keys-to-remove)))
     cl-structures)
    ;; (message "Removed structures %s" keys-to-remove)
    (dolist (key keys-to-remove)
      (remhash key cl-structures))))

;; (elsa-state-get-structure :: (function ((struct elsa-declarations) symbol) (struct elsa-cl-structure)))
(cl-defgeneric elsa-state-get-structure (this name)
  "Get from THIS the `elsa-cl-structure' definition called NAME.")

(cl-defgeneric elsa-state-add-defun ((this elsa-declarations) (def elsa-defun))
  "Add `elsa-defun' to state object."
  (declare (indent 1))
  (oset def file (elsa-state-current-file this))
  (put (oref def name) 'elsa-type (oref def type))
  (puthash (oref def name) def (oref this defuns)))

(cl-defgeneric elsa-state-clear-file-defuns ((this elsa-declarations) file)
  "Remove from THIS state all functions declared in FILE."
  (let ((keys-to-remove nil)
        (defuns (oref this defuns)))
    (maphash
     (lambda (key value)
       (when (equal (oref value file) file)
         (push key keys-to-remove)))
     defuns)
    ;; (message "Removed defuns %s" keys-to-remove)
    (dolist (key keys-to-remove)
      (remhash key defuns))))

;; (elsa-state-get-defun :: (function ((struct elsa-declarations) symbol) (struct elsa-defun)))
(cl-defgeneric elsa-state-get-defun (this name)
  "Get from THIS the `elsa-defun' definition called NAME.")

(cl-defgeneric elsa-state-add-defvar ((this elsa-declarations) (def elsa-defvar))
  "Add `elsa-defvar' to state object."
  (declare (indent 1))
  (oset def file (elsa-state-current-file this))
  (put (oref def name) 'elsa-type-var (oref def type))
  (puthash (oref def name) def (oref this defvars)))

(cl-defgeneric elsa-state-clear-file-defvars ((this elsa-declarations) file)
  "Remove from THIS state all variables declared in FILE."
  (let ((keys-to-remove nil)
        (defvars (oref this defvars)))
    (maphash
     (lambda (key value)
       (when (equal (oref value file) file)
         (push key keys-to-remove)))
     defvars)
    (dolist (key keys-to-remove)
      (remhash key defvars))))

;; (elsa-state-get-defvar :: (function ((struct elsa-declarations) symbol) (struct elsa-defvar)))
(cl-defgeneric elsa-state-get-defvar ((this elsa-declarations) (name symbol))
  "Get from THIS the `elsa-defvar' definition called NAME.")

(cl-defgeneric elsa-state-clear-file-state ((this elsa-declarations) file)
  (elsa-state-clear-file-defuns this file)
  (elsa-state-clear-file-defvars this file)
  (elsa-state-clear-file-structures this file))

(defclass elsa-global-state (elsa-declarations)
  ((project-directory :type string
                      :initarg :project-directory
                      :documentation "Directory from which the analysis was started.")
   (current-file :type (or string null)
                 :initarg :current-file
                 :initform nil
                 :documentation "File being processed.")
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

(cl-defgeneric elsa-state-current-file (this)
  "Return currently processed file.")

(cl-defmethod elsa-state-current-file ((this elsa-global-state))
  (oref this current-file))

(cl-defmethod elsa-state-get-defun ((this elsa-global-state) name)
  (gethash name (oref this defuns)))

(cl-defmethod elsa-state-get-defvar ((this elsa-global-state) (name symbol))
  (gethash name (oref this defvars)))

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
  ((errors :initform nil)
   (provide :initform nil
            :documentation "Symbols provided by current file")
   (requires :initform nil :type list
             :documentation "Symbols required by current file")
   (dependencies :initform nil :type list
                 :documentation "List of all recursively processed dependencies")
   (ignored-lines :initform nil)
   (reachable :initform (list (trinary-true)))
   ;; TODO: I don't remember or understand what this is.  I'm going to
   ;; commit it since I can't see it making any difference.  But it
   ;; probably serves some purpose.
   (quoted :initform (trinary-false))
   (scope :initform (elsa-scope))
   (lsp-method :initarg :lsp-method :initform nil)
   (lsp-params :initarg :lsp-params :initform nil)
   (global-state :type elsa-global-state
                 :initarg :global-state
                 :initform (elsa-global-state))))

(cl-defmethod elsa-state-update-global ((this elsa-state) (global elsa-global-state))
  "Copy declarations from THIS local state to GLOBAL state."
  (maphash (lambda (_name def)
             (elsa-state-add-defun global def))
           (oref this defuns))
  (maphash (lambda (_name def)
             (elsa-state-add-defvar global def))
           (oref this defvars))
  (maphash (lambda (_name def)
             (elsa-state-add-structure global def))
           (oref this cl-structures)))

(cl-defmethod elsa-state-current-file ((this elsa-state))
  (oref (oref this global-state) current-file))

(cl-defmethod elsa-state-get-defun ((this elsa-state) name)
  (or (gethash name (oref this defuns))
      (elsa-state-get-defun (oref this global-state) name)))

(cl-defmethod elsa-state-get-defvar ((this elsa-state) (name symbol))
  (or (gethash name (oref this defvars))
      (elsa-state-get-defvar (oref this global-state) name)))

(cl-defmethod elsa-state-get-structure ((this elsa-state) name)
  (or (gethash name (oref this cl-structures))
      (elsa-state-get-structure (oref this global-state) name)))

(cl-defmethod elsa-state-ignore-line ((state elsa-state) line)
  (push line (oref state ignored-lines)))

;; (elsa-state-add-message :: (function ((struct elsa-state) (struct elsa-message)) nil))
(defun elsa-state-add-message (state error)
  "In STATE, record an ERROR.

STATE is `elsa-state', ERROR is `elsa-message'."
  (declare (indent 1))
  (unless (memq (oref error line) (oref state ignored-lines))
    (push error (oref state errors))))

(cl-defmethod elsa-state-get-var-symbols ((this elsa-state))
  "Return all symbols in the workspace representing variables."
  (append (hash-table-keys (oref this defvars))
          (hash-table-keys (oref (oref this global-state) defvars))))

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
