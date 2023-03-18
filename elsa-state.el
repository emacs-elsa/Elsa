(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))

(require 'trinary)

(require 'elsa-form)
(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-error)
(require 'elsa-methods)
(require 'elsa-structure-slot)

(defclass elsa-structure nil
  (
   ;; (slots :: (hash-table symbol (class elsa-structure-slot)))
   (slots
    :type hash-table
    :initarg :slots
    :initform (make-hash-table)
    :documentation "Slots available on this structure.

Does not include slots on parents.")
   (parents
    :type list
    :initarg :parents
    :initform nil
    :documentation "Tree of parents of this structure."))
  :documentation "Representation of structured object.

Structured objects have slots and parents.")

;; (elsa-get-parents :: (function (mixed) (list (class elsa-structure))))
(cl-defgeneric elsa-get-parents (this)
  "Get all parents of THIS structure.")

;; (elsa-get-slots :: (function (mixed) (list (class elsa-structure-slot))))
(cl-defgeneric elsa-get-slots (this)
  "Get slots from THIS structure or structured type.")

;; (elsa-get-slot :: (function (mixed symbol) (or nil (class elsa-structure-slot))))
(cl-defgeneric elsa-get-slot (this name)
  "Get slot definition NAME from THIS.")

(cl-defmethod elsa-get-slots ((this elsa-structure))
  (-mapcat
   (lambda (par) (hash-table-values (oref par slots)))
   (elsa-get-parents this)))

(cl-defmethod elsa-get-slot ((this elsa-structure) (name symbol))
  (-some
   (lambda (par) (gethash name (oref par slots)))
   (elsa-get-parents this)))

(defclass elsa-declaration nil
  ((name
    :type symbol
    :initarg :name
    :documentation "Declaration name.")
   (file
    :type (or string null)
    :initarg :file
    :documentation "File where the declaration was declared."))
  :abstract t
  :documentation "Abstract declaration.

Each declaration such as defun, defvar or defclass has a name and file
where it was declared.")

(defclass elsa-defstruct (elsa-declaration elsa-structure)
  ()
  :documentation "Representation of a `cl-defstruct'.")

(cl-defmethod elsa-get-parents ((this elsa-defstruct))
  (->> (oref this parents)
       (mapcar #'car)
       (-keep (lambda (parent-name)
                (get parent-name 'elsa-defstruct)))))

(defclass elsa-defclass (elsa-declaration elsa-structure)
  ()
  :documentation "Representation of an EIEIO `defclass'.")

(cl-defmethod elsa-get-parents ((this elsa-defclass))
  (->> (oref this parents)
       (mapcar #'car)
       (-keep (lambda (parent-name)
                (get parent-name 'elsa-defclass)))))

(defclass elsa-defun (elsa-declaration)
  ((type
    :type elsa-type
    :initarg :type
    :documentation "Function type of the defun.")
   (defun-type
     :type symbol
     :initarg :defun-type
     :initform 'defun
     :documentation "Type of the definition.

Can be defun, defmacro, cl-defmethod, cl-defgeneric.")
   (defgeneric-type
     :type (or elsa-type null)
     :initarg :defgeneric-type
     :initform nil
     :documentation "Type of the cl-defgeneric.

If the defun-type is `cl-defgeneric' or `cl-defmethod' any new
overload must be compatible with defgeneric-type.

We also narrow the return type of an overload to the intersection of
its returntype and defgeneric-type's return type.")
   (arglist
    :initarg :arglist
    :documentation "Defun arglist."))
  :documentation "Defun and defun-like definitions discovered during analysis.")

(cl-defmethod elsa-get-type ((this elsa-defun))
  (oref this type))

(cl-defmethod elsa-type-get-return ((this elsa-defun))
  (elsa-type-get-return (oref this type)))

(cl-defmethod elsa-type-get-generic-return ((this elsa-defun))
  (when-let ((defgeneric-type (oref this defgeneric-type)))
    (elsa-type-get-return defgeneric-type)))

(defclass elsa-defvar (elsa-declaration)
  ((type
    :type elsa-type
    :initarg :type))
  :documentation "Defvar and defvar-like declarations.

`defcustom' behaves exactly as `defvar' except we can automatically
defive the type from the custom type declaration.

`defconst' is like `defvar' but the type is automatically set to
readonly.")

(cl-defmethod elsa-get-type ((this elsa-defvar))
  (oref this type))

(defclass elsa-declarations nil
  (
   ;; (defuns :: (hash-table symbol (class elsa-defun)))
   (defuns
     :type hash-table
     :initform (make-hash-table)
     :documentation "Defun-like declarations.")
   ;; (defvars :: (hash-table symbol (class elsa-defvar)))
   (defvars
     :type hash-table
     :initform (make-hash-table)
     :documentation "Defvar-like declarations.")
   ;; (defstructs :: (hash-table symbol (class elsa-defstruct)))
   (defstructs
    :type hash-table
    :initform (make-hash-table)
    :documentation "Structure-like declarations.")
   ;; (defclasses :: (hash-table symbol (class elsa-defclass)))
   (defclasses
    :type hash-table
    :initform (make-hash-table)
    :documentation "Class-like declarations."))
  :documentation "Class holding discovered declarations.")

(cl-defgeneric elsa-state-add-defstruct ((this elsa-declarations) (str elsa-defstruct))
  "Add `elsa-defstruct' to state object."
  (declare (indent 1))
  (oset str file (elsa-state-current-file this))
  (put (oref str name) 'elsa-defstruct str)
  (puthash (oref str name) str (oref this defstructs)))

(cl-defgeneric elsa-state-clear-file-structures ((this elsa-declarations) file)
  "Remove from THIS state all structures declared in FILE."
  (let ((keys-to-remove nil)
        (defstructs (oref this defstructs)))
    (maphash
     (lambda (key value)
       (when (equal (oref value file) file)
         (push key keys-to-remove)))
     defstructs)
    ;; (message "Removed defstructs %s" keys-to-remove)
    (dolist (key keys-to-remove)
      (remhash key defstructs))))

;; (elsa-state-get-defstruct :: (function ((class elsa-declarations) symbol) (or (class elsa-defstruct) nil)))
(cl-defgeneric elsa-state-get-defstruct (this name)
  "Get from THIS the `elsa-defstruct' definition called NAME.")

(cl-defgeneric elsa-state-add-defclass ((this elsa-declarations) (str elsa-defclass))
  "Add `elsa-defclass' to state object."
  (declare (indent 1))
  (oset str file (elsa-state-current-file this))
  (put (oref str name) 'elsa-defclass str)
  (puthash (oref str name) str (oref this defclasses)))

(cl-defgeneric elsa-state-clear-file-classes ((this elsa-declarations) file)
  "Remove from THIS state all classes declared in FILE."
  (let ((keys-to-remove nil)
        (defclasses (oref this defclasses)))
    (maphash
     (lambda (key value)
       (when (equal (oref value file) file)
         (push key keys-to-remove)))
     defclasses)
    ;; (message "Removed defclasses %s" keys-to-remove)
    (dolist (key keys-to-remove)
      (remhash key defclasses))))

;; (elsa-state-get-defclass :: (function ((class elsa-declarations) symbol) (or (class elsa-defclass) nil)))
(cl-defgeneric elsa-state-get-defclass (this name)
  "Get from THIS the `elsa-defclass' definition called NAME.")

(cl-defgeneric elsa-state-add-defun ((this elsa-declarations) (def elsa-defun))
  "Add `elsa-defun' to state object."
  (declare (indent 1))
  (oset def file (elsa-state-current-file this))
  (put (oref def name) 'elsa-type (oref def type))
  (puthash (oref def name) def (oref this defuns)))

(cl-defgeneric elsa-state-remove-defun ((this elsa-declarations) (name symbol))
  "Remove `elsa-defun' from state object."
  (declare (indent 1))
  (put name 'elsa-type nil)
  (remhash name (oref this defuns)))

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

;; (elsa-state-get-defun :: (function ((class elsa-declarations) symbol) (or (class elsa-defun) nil)))
(cl-defgeneric elsa-state-get-defun (this name)
  "Get from THIS the `elsa-defun' definition called NAME.")

;; (elsa-state-add-method :: (function ((class elsa-declarations) (class elsa-defun)) mixed))
(defun elsa-state-add-method (this method)
  (declare (indent 1))
  (let ((def (elsa-state-get-defun this (oref method name))))
    (if (not def)
        (progn
          (when (and (eq (oref method defun-type) 'cl-defgeneric)
                     (not (oref method defgeneric-type)))
            (oset method defgeneric-type (oref method type)))
          (elsa-state-add-defun this method))
      (pcase (oref method defun-type)
        ('cl-defmethod
          (let ((expected-return-type
                 (or (elsa-type-get-generic-return def)
                     (elsa-type-mixed))))
            ;; If the definition already exist, intersect the function
            ;; type to add new "overload".
            (oset def type (elsa-type-intersect (oref method type) (oref def type)))))
        (`defun
            ;; for defun, just replace the existing type.
            (oset def type (oref method type))))
      ;; We also need to re-add it to update the symbol and file
      ;; definition pointer for caching and "go to definition".
      (elsa-state-add-defun this def))))

(cl-defgeneric elsa-state-add-defvar ((this elsa-declarations) (def elsa-defvar))
  "Add `elsa-defvar' to state object."
  (declare (indent 1))
  ;; FIXME: the call to `elsa-state-current-file' is not safe.
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

;; (elsa-state-get-defvar :: (function ((class elsa-declarations) symbol) (or (class elsa-defvar) nil)))
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
                    :documentation "Total number of files to analyze.")
   (max-file-name-length
    :type integer
    :documentation "Maximum file name length of all the processed files.

This is mostly used for output formatting."))
  :documentation "Initial configuration and state of the entire analysis.

The other state class `elsa-state' state holds state of an analysis of
a single file, form or set of forms.")

(defvar elsa-global-state (elsa-global-state)
  "Global state holding elsa analysis context.")

(cl-defgeneric elsa-state-current-file (this)
  "Return currently processed file.")

(cl-defmethod elsa-state-current-file ((this elsa-global-state))
  (oref this current-file))

(cl-defmethod elsa-state-get-defun ((this elsa-global-state) name)
  (gethash name (oref this defuns)))

(cl-defmethod elsa-state-get-defvar ((this elsa-global-state) (name symbol))
  (gethash name (oref this defvars)))

(cl-defmethod elsa-state-get-defstruct ((this elsa-global-state) name)
  (gethash name (oref this defstructs)))

(cl-defmethod elsa-state-get-defclass ((this elsa-global-state) name)
  (gethash name (oref this defclasses)))

(cl-defmethod elsa-global-state-get-counter ((this elsa-global-state))
  (let ((max-len (length (number-to-string (oref this number-of-files)))))
    (format (format "%%%dd/%%%dd" max-len max-len)
            (oref this processed-file-index)
            (oref this number-of-files))))

;; (elsa-global-state-prefix-length :: (function ((class elsa-global-state) (or int nil)) int))
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
   (reachable
    :initform (list (trinary-true))
    :documentation "Is this form reachable during execution?

This field is used during the analysis step.")
   (no-expand
    :type boolean
    :initform nil
    :documentation "Whether we are in an expanded macro.

We should not try to instrument already instrumented forms.")
   ;; TODO: I don't remember or understand what this is.  I'm going to
   ;; commit it since I can't see it making any difference.  But it
   ;; probably serves some purpose.
   (quoted :initform (trinary-false))
   (scope :initform (elsa-scope))
   (lsp-method :initarg :lsp-method :initform nil)
   (lsp-params :initarg :lsp-params :initform nil)
   (lsp-analyzer :initarg :lsp-analyzer :initform nil)
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
             (elsa-state-add-defstruct global def))
           (oref this defstructs))
  (maphash (lambda (_name def)
             (elsa-state-add-defclass global def))
           (oref this defclasses)))

(cl-defmethod elsa-state-current-file ((this elsa-state))
  (oref (oref this global-state) current-file))

(cl-defmethod elsa-state-get-defun ((this elsa-state) name)
  (or (gethash name (oref this defuns))
      (elsa-state-get-defun (oref this global-state) name)))

(cl-defmethod elsa-state-get-defvar ((this elsa-state) (name symbol))
  (or (gethash name (oref this defvars))
      (elsa-state-get-defvar (oref this global-state) name)))

(cl-defmethod elsa-state-get-defstruct ((this elsa-state) name)
  (or (gethash name (oref this defstructs))
      (elsa-state-get-defstruct (oref this global-state) name)))

(cl-defmethod elsa-state-get-defclass ((this elsa-state) name)
  (or (gethash name (oref this defclasses))
      (elsa-state-get-defclass (oref this global-state) name)))

(cl-defmethod elsa-state-ignore-line ((state elsa-state) line)
  (push line (oref state ignored-lines)))

;; (elsa-state-add-message :: (function ((class elsa-state) (class elsa-message)) nil))
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

;; (elsa-state-get-reachability :: (function ((class elsa-state)) (class trinary)))
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

;; (elsa-state-quoted-p :: (function ((class elsa-state)) bool))
(defun elsa-state-quoted-p (state)
  "Return non-nil if the current form is quoted."
  (trinary-true-p (oref state quoted)))

(provide 'elsa-state)
