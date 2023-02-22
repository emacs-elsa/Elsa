;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'pcase)
(require 'backquote)
(require 'seq)

(require 'trinary)

(require 'elsa-error)
(require 'elsa-state)
(require 'elsa-types)
(require 'elsa-type-helpers)
(require 'elsa-methods)

(defun elsa--skip-whitespace-forward ()
  (while (forward-comment 1))
  (skip-chars-forward " \t\n\r"))

(defun elsa--improper-list-p (cell)
  "Return non-nil if CELL is an improper list."
  (let ((len (safe-length cell)))
    (when (and (consp cell)
               (> len 0)
               (not (listp (nthcdr len cell))))
      (nthcdr len cell))))

(defsubst elsa--quote-p (symbol)
  "Return non-nil if SYMBOL is a type of quote."
  (memq symbol (list
                'function
                'quote
                backquote-backquote-symbol
                backquote-unquote-symbol
                backquote-splice-symbol)))

;; (elsa--forward-sexp :: (function ((or int nil)) int))
(defsubst elsa--forward-sexp (&optional n)
  "Skip `forward-sexp' N times and return `point'."
  (setq n (or n 1))
  (forward-sexp n)
  (point))

(defclass elsa-form nil
  ((start :type integer :initarg :start)
   (end :type integer :initarg :end)
   (quote-type :type symbol :initarg :quote-type :initform nil)
   (line :type integer :initarg :line)
   (column :type integer :initarg :column)
   (end-line :type integer :initarg :end-line)
   (end-column :type integer :initarg :end-column)
   (type :type elsa-type :initarg :type :initform (elsa-make-type mixed))
   (narrow-types :initarg :narrow-type :initform nil)
   (reachable :type trinary :initarg :reachable :initform (trinary-true))
   (parent :type (or elsa-form nil) :initarg :parent)
   (annotation :type list :initarg :annotation :initform nil))
  :abstract t)

;; TODO: this will be easier to do later when we properly push quoted
;; types up.  The check will then be just for a symbol and a quote
(defun elsa--quoted-symbol-p (form &optional quote-types)
  "Return non-nil if FORM represents a quoted symbol."
  (setq quote-types (or quote-types (list 'quote)))
  (when (memq (oref form quote-type) quote-types)
    (-when-let (seq (elsa-form-sequence form))
      (when (= (length seq) 2)
        (elsa-form-symbol-p (cadr seq))))))

(defun elsa--quoted-symbol-name (form)
  "Return the name of quoted symbol that FORM represents.

Nil if FORM is not a quoted symbol."
  (when (elsa--quoted-symbol-p form)
    (elsa-get-name (cadr (elsa-form-sequence form)))))

;; (elsa-form-sequence :: (function (mixed) (list mixed)))
(cl-defgeneric elsa-form-sequence (form)
  "Return the sequence of things contained in FORM.")

(cl-defmethod elsa-scope-narrow-var ((scope elsa-scope) (form elsa-form) &optional updater)
  (elsa-scope-narrow-var scope (oref form narrow-types) updater))

(cl-defmethod elsa-get-type ((this elsa-form))
  (oref this type))

(cl-defmethod elsa-type-sum ((this elsa-type) (other elsa-form))
  (elsa-type-sum this (oref other type)))

(cl-defmethod elsa-type-sum ((this elsa-form) (other elsa-form))
  (elsa-type-sum (oref this type) (oref other type)))

(cl-defmethod elsa-type-sum ((this elsa-form) (other elsa-type))
  (elsa-type-sum (oref this type) other))

(cl-defmethod elsa-type-intersect ((this elsa-type) (other elsa-form))
  (elsa-type-intersect this (oref other type)))

(cl-defmethod elsa-type-intersect ((this elsa-form) (other elsa-form))
  (elsa-type-intersect (oref this type) (oref other type)))

(cl-defmethod elsa-type-intersect ((this elsa-form) (other elsa-type))
  (elsa-type-intersect (oref this type) other))

(cl-defmethod elsa-type-diff ((this elsa-type) (other elsa-form))
  (elsa-type-diff this (oref other type)))

(cl-defmethod elsa-type-diff ((this elsa-form) (other elsa-form))
  (elsa-type-diff (oref this type) (oref other type)))

(cl-defmethod elsa-type-diff ((this elsa-form) (other elsa-type))
  (elsa-type-diff (oref this type) other))

(cl-defmethod elsa-type-accept ((this elsa-form) (other elsa-form))
  (elsa-type-accept (elsa-get-type this) (oref other type)))

(cl-defmethod elsa-type-accept ((this elsa-type) (other elsa-form))
  (elsa-type-accept this (oref other type)))

(cl-defmethod elsa-type-accept ((this elsa-form) (other elsa-type))
  (elsa-type-accept (oref this type) other))

(cl-defmethod elsa-type-is-nil ((condition elsa-form))
  (elsa-type-is-nil (oref condition type)))

(cl-defmethod elsa-type-is-non-nil ((condition elsa-form))
  (elsa-type-is-non-nil (oref condition type)))

(cl-defgeneric elsa-form-print ((this elsa-form))
  "Print THIS form in a way that can be read back in.

This function does not prettyprint.

Each class should implement more efficient print method if
possible since format has some overhead parsing the specification
and so on."
  (format "%s" this))

(cl-defmethod elsa-tostring ((this elsa-form))
  (elsa-form-print this))

(cl-defmethod cl-print-object ((this elsa-form) stream)
  (princ (elsa-tostring this) stream))

(cl-defgeneric elsa-form-to-lisp ((this elsa-form))
  "Return this form as lisp form."
  (error "Not implemented for form: %S" this))

(cl-defmethod elsa-form-length ((this elsa-form))
  (- (oref this end) (oref this start)))

(defun elsa-form-reachable (form)
  (oref form reachable))

(cl-defgeneric elsa-form-visit ((this elsa-form) fn)
  "Visit each node of THIS elsa-form and call FN.

If the form is a cons, list or vector, recurse into the child
nodes."
  (declare (indent 1))
  (funcall fn this))

(cl-defgeneric elsa-form-foreach (_elsa-form _fn)
  "For each item of ELSA-FORM execute FN with the item as first argument.

This only makes sense for the sequence forms:

- `elsa-form-vector'
- `elsa-form-list'
- `elsa-form-improper-list'"
  (declare (indent 1))
  nil)

(cl-defgeneric elsa-form-map (_elsa-form _fn)
  "Map each item of ELSA-FORM through FN with the item as first argument.

This only makes sense for the sequence forms:

- `elsa-form-vector'
- `elsa-form-list'
- `elsa-form-improper-list'"
  (declare (indent 1))
  nil)

;;; Atoms
(defclass elsa-form-atom (elsa-form)
  nil
  :abstract t)

(defclass elsa-form-symbol (elsa-form-atom)
  ((name :type symbol :initarg :name)))

(cl-defmethod elsa-form-print ((this elsa-form-symbol))
  (symbol-name (oref this name)))

(cl-defmethod elsa-form-to-lisp ((this elsa-form-symbol))
  (oref this name))

(cl-defmethod elsa-form-sequence ((this elsa-form-symbol))
  (if (eq (elsa-get-name this) 'nil)
      nil
    (error "Can not get sequence out of symbol form")))

(cl-defgeneric elsa-form-sequence-p (_this)
  nil)

(cl-defmethod elsa-form-sequence-p ((this elsa-form-symbol))
  (eq (elsa-get-name this) 'nil))

(cl-defmethod elsa-scope-remove-var ((this elsa-scope) (form elsa-form-symbol))
  "Remove VARIABLE from current scope."
  (elsa-scope--remove-var this (elsa-get-name form)))

(cl-defmethod elsa-scope-get-var ((this elsa-scope) (form elsa-form-symbol))
  "Get binding of FORM in THIS scope."
  (elsa-scope--get-var this (elsa-get-name form)))

(defconst elsa--syntax-table-for-symbols
  (let ((table (make-syntax-table
                (with-temp-buffer
                  (emacs-lisp-mode)
                  (syntax-table)))))
    (modify-syntax-entry ?# "_" table)
    table)
  "Syntax table used to read symbols.

This prevents errors when someone names a symbol #### or some
other syntax, where the constituents are all in syntax class
prefix and skipped by the sexp scanner.")

(defsubst elsa--read-symbol (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-symbol
   :start (point)
   :name form
   :end (with-syntax-table elsa--syntax-table-for-symbols
          (elsa--forward-sexp))))

(defclass elsa-form-keyword (elsa-form-symbol) ())

(cl-defmethod elsa-form-print ((this elsa-form-keyword))
  (symbol-name (oref this name)))

(cl-defmethod elsa-form-to-lisp ((this elsa-form-keyword))
  (oref this name))

;; (elsa-form-function-call-p :: (function (mixed (or symbol nil)) bool))
(cl-defgeneric elsa-form-function-call-p (_this &optional _name) nil)

(cl-defmethod elsa-get-name ((this elsa-form-symbol))
  (oref this name))

(defsubst elsa--read-keyword (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-keyword
   :type (elsa-const-type
          :type (elsa-make-type keyword)
          :value form)
   :start (point)
   :name form
   :end (elsa--forward-sexp)))

(defclass elsa-form-number (elsa-form-atom)
  ((value :type number :initarg :value)))

(cl-defmethod elsa-form-print ((this elsa-form-number))
  (number-to-string (oref this value)))

(cl-defmethod elsa-form-to-lisp ((this elsa-form-number))
  (oref this value))

(defclass elsa-form-integer (elsa-form-number)
  ((value :initarg :value)))

(defsubst elsa--read-integer (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-integer
   :type (elsa-const-type
          :type (elsa-make-type int)
          :value form)
   :start (point)
   :value form
   :end (progn
          ;; We use read again to handle the ? character syntax which
          ;; is very tricky
          (read (current-buffer))
          (point))))

(defclass elsa-form-float (elsa-form-number)
  ((value :initarg :value)))

(defsubst elsa--read-float (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-float
   :type (elsa-const-type
          :type (elsa-make-type float)
          :value form)
   :start (point)
   :value form
   :end (elsa--forward-sexp)))

;;;; Sequences
(defclass elsa-form-seq (elsa-form-atom)
  nil
  :abstract t)

(defclass elsa-form-string (elsa-form-seq)
  ((sequence :type string :initarg :sequence)))

(cl-defmethod elsa-form-sequence ((this elsa-form-string))
  (oref this sequence))

(cl-defmethod elsa-form-print ((this elsa-form-string))
  (format "%S" (oref this sequence)))

(cl-defmethod elsa-form-to-lisp ((this elsa-form-string))
  (oref this sequence))

(defsubst elsa--read-string (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-string
   :type (elsa-const-type
          :type (elsa-make-type string)
          :value form)
   :start (point)
   :end (elsa--forward-sexp)
   :sequence form))

(defclass elsa-form-vector (elsa-form-seq)
  ((sequence :type list :initarg :sequence)))

(cl-defmethod elsa-form-print ((this elsa-form-vector))
  (format "[%s]" (mapconcat 'elsa-form-print (oref this sequence) " ")))

(cl-defmethod elsa-form-to-lisp ((this elsa-form-vector))
  (apply #'vector (mapcar #'elsa-form-to-lisp (oref this sequence))))

(cl-defmethod elsa-form-foreach ((this elsa-form-vector) fn)
  (mapc fn (oref this sequence)))

(cl-defmethod elsa-form-map ((this elsa-form-vector) fn)
  (mapcar fn (oref this sequence)))

(cl-defmethod elsa-form-visit ((this elsa-form-vector) fn)
  (funcall fn this)
  (elsa-form-foreach this (lambda (x) (elsa-form-visit x fn))))

(cl-defmethod elsa-form-sequence ((this elsa-form-vector))
  (oref this sequence))

(defsubst elsa--read-vector (form state)
  (elsa--skip-whitespace-forward)
  (elsa-form-vector
   :type (elsa-make-type (vector mixed))
   :start (prog1 (point) (down-list))
   :sequence (-map (lambda (f) (elsa--read-form f state)) form)
   :end (progn (up-list) (point))))

;;; Conses
(defclass elsa-form-cons (elsa-form) ())

(defclass elsa-form-list (elsa-form-cons elsa-form-seq)
  ((sequence :type list :initarg :sequence)))

(cl-defmethod elsa-form-print ((this elsa-form-list))
  (format "(%s)" (mapconcat 'elsa-form-print (oref this sequence) " ")))

(cl-defmethod elsa-form-print ((this list))
  (format "(%s)" (mapconcat 'elsa-form-print this " ")))

(cl-defmethod elsa-form-to-lisp ((this elsa-form-list))
  (apply #'list (mapcar #'elsa-form-to-lisp (oref this sequence))))

(cl-defmethod elsa-form-to-lisp ((this list))
  (apply #'list (mapcar #'elsa-form-to-lisp this)))

(cl-defmethod elsa-form-foreach ((this elsa-form-list) fn)
  (mapc fn (oref this sequence)))

(cl-defmethod elsa-form-foreach ((this list) fn)
  (mapc fn this))

(cl-defmethod elsa-form-map ((this elsa-form-list) fn)
  (mapcar fn (oref this sequence)))

(cl-defmethod elsa-form-visit ((this elsa-form-list) fn)
  (funcall fn this)
  (elsa-form-foreach this (lambda (x) (elsa-form-visit x fn))))

;; (elsa-car :: (function (mixed) mixed))
(cl-defgeneric elsa-car (thing)
  "Return `car' of THING")

(cl-defmethod elsa-car ((this list))
  (car this))

(cl-defmethod elsa-car ((this elsa-form))
  (car (elsa-form-sequence this)))

;; (elsa-cdr :: (function (mixed) (list mixed)))
(cl-defgeneric elsa-cdr (thing)
  "Return `cdr' of THING")

(cl-defmethod elsa-cdr ((this list))
  (cdr this))

(cl-defmethod elsa-cdr ((this elsa-form))
  (cdr (elsa-form-sequence this)))

;; (elsa-cadr :: (function (mixed) mixed))
(cl-defgeneric elsa-cadr (thing)
  "Return `cadr' of THING")

(cl-defmethod elsa-cadr ((this list))
  (cadr this))

(cl-defmethod elsa-cadr ((this elsa-form))
  (cadr (elsa-form-sequence this)))

;; (elsa-nth :: (function (int mixed) mixed))
(cl-defgeneric elsa-nth (n thing)
  "Return nth item of THING")

(cl-defmethod elsa-nth (n (this list))
  (nth n this))

(cl-defmethod elsa-nth (n (this elsa-form))
  (nth n (elsa-form-sequence this)))

;; (elsa-nthcdr :: (function (int mixed) (list mixed)))
(cl-defgeneric elsa-nthcdr (n thing)
  "Return nth `cdr' of THING")

(cl-defmethod elsa-nthcdr (n (this list))
  (nthcdr n this))

(cl-defmethod elsa-nthcdr (n (this elsa-form))
  (nthcdr n (elsa-form-sequence this)))

(cl-defmethod elsa-form-sequence ((this elsa-form-list))
  (oref this sequence))

(cl-defmethod elsa-form-sequence ((this list))
  this)

(cl-defmethod elsa-form-sequence-p ((_this elsa-form-list)) t)

(cl-defmethod elsa-get-name ((this elsa-form-list))
  (-when-let (head (elsa-car this))
    (elsa-get-name head)))

(cl-defmethod elsa-form-function-call-p ((this elsa-form-list) &optional name)
  (-when-let (head (elsa-car this))
    (and (elsa-form-symbol-p head)
         (or (not name)
             (eq (oref head name) name)))))

(defclass elsa-form-improper-list (elsa-form-cons)
  ((conses :type list :initarg :conses)))

(cl-defmethod elsa-form-print ((this elsa-form-improper-list))
  (let* ((seq (oref this conses))
         (len (safe-length seq))
         (prefix (seq-take seq len))
         (last (cdr (last seq))))
    (format "(%s . %s)"
            (mapconcat 'elsa-form-print prefix " ")
            (elsa-form-print last))))

(cl-defmethod elsa-form-to-lisp ((this elsa-form-improper-list))
  (let* ((seq (oref this conses))
         (len (safe-length seq))
         (prefix (seq-take seq len))
         (last (cdr (last seq))))
    (-reduce-r-from
     (lambda (it acc)
       (cons (elsa-form-to-lisp it) acc))
     (elsa-form-to-lisp last)
     prefix)))

(cl-defmethod elsa-form-foreach ((this elsa-form-improper-list) fn)
  (let* ((seq (oref this conses))
         (len (safe-length seq))
         (prefix (seq-take seq len))
         (last (cdr (last seq))))
    (mapc fn (-snoc prefix last))))

(cl-defmethod elsa-form-map ((this elsa-form-improper-list) fn)
  (let* ((seq (oref this conses))
         (len (safe-length seq))
         (prefix (seq-take seq len))
         (last (cdr (last seq))))
    (mapcar fn (-snoc prefix last))))

(cl-defmethod elsa-form-visit ((this elsa-form-improper-list) fn)
  (funcall fn this)
  (elsa-form-foreach this (lambda (x) (elsa-form-visit x fn))))

(cl-defmethod elsa-car ((this elsa-form-improper-list))
  (car (oref this conses)))

(cl-defmethod elsa-cdr ((this elsa-form-improper-list))
  (cdr (oref this conses)))

;; (elsa--read-cons :: (function ((list mixed) mixed) mixed))
(defsubst elsa--read-cons (form state)
  (elsa--skip-whitespace-forward)
  (if (elsa--improper-list-p form)
      (elsa-form-improper-list
       :type (elsa-make-type (list mixed))
       :start (prog1 (point) (down-list))
       :conses (let ((depth 0)
                     (items))
                 (while (consp form)
                   (let ((head (elsa--read-form (car form) state)))
                     (push head items)
                     (!cdr form)
                     (elsa--skip-whitespace-forward)
                     (when (looking-at-p "\\.[^[:alnum:].]")
                       (forward-char 1)
                       (if (consp form)
                           (progn
                             (cl-incf depth)
                             (down-list))
                         (push (elsa--read-form form state) items)))))
                 (while (>= (cl-decf depth) 0) (up-list))
                 (apply 'cl-list* (nreverse items)))
       :end (progn (up-list) (point)))
    (elsa-form-list
     :start (prog1 (point) (down-list))
     :sequence
      (let ((depth 0)
            (items))
        (while form
          (cond
           ((elsa--quote-p (car form))
            (let ((quoted-form (elsa--read-form form state)))
              (setq items
                    (-concat (reverse (oref quoted-form sequence))
                             items)))
            (setq form nil))
           (t
            (push (elsa--read-form (car form) state) items)
            (!cdr form)))
          (elsa--skip-whitespace-forward)
          (when (and form (looking-at-p "\\.[^[:alnum:].]"))
            (if (elsa--quote-p (car form))
                (forward-sexp) ;; skip the dot
              (cl-incf depth)
              (down-list))))
        (while (>= (cl-decf depth) 0) (up-list))
        (nreverse items))
      :end (progn (up-list) (point)))))

(defclass elsa-form-function (elsa-form)
  ((function :type function :initarg :function)))

(cl-defmethod elsa-form-print ((this elsa-form-function))
  (format "%S" (oref this function)))

(cl-defmethod elsa-form-to-lisp ((this elsa-form-function))
  (oref this function))

(defun elsa--read-function (form _state)
  (elsa--skip-whitespace-forward)
  (elsa-form-function
   :type (elsa-make-type (function (&rest mixed) mixed))
   :start (point)
   :function form
   :end (progn (forward-sexp) (point))))

(defun elsa--read-quote (form state)
  (elsa--skip-whitespace-forward)
  (let* ((expanded-form nil)
         (start (point))
         (seq (cons
               (elsa--set-line-and-column
                (elsa-form-symbol
                 :start (cond
                         ((looking-at (rx (or (and (? "#") "'") "," "`" ",@")))
                          (prog1 (point)
                            (forward-char (length (match-string 0)))))
                         ;; when we write (quote foo) instead of 'foo
                         ((looking-at-p "(")
                          (setq expanded-form t)
                          (down-list)
                          (prog1 (point)
                            (forward-sexp)))
                         (t (prog1 (point)
                              (forward-char (length (symbol-name (car form)))))))
                 :name (car form)
                 :end (point)))
               (-map (lambda (f) (elsa--read-form f state))
                     ;; make sure to make list here, because we might
                     ;; be reading a "quoted quote" where the structure
                     ;; can be for example an alist with key
                     ;; `function' or `quote' and value anything,
                     ;; including just a symbol.
                     (-list (cdr form))))))
    (elsa-form-list
     :quote-type (car form)
     :start start
     :sequence seq
     :end (progn
            (when expanded-form (up-list))
            (point)))))

(defsubst elsa--process-annotation (reader-form comment-form state)
  "Process annotation over a form.

READER-FORM is an `elsa-form' instance representing the form
after the annotation.

COMMENT-FORM is a regular lisp form (returned from `read').

STATE is Elsa local state."
  (cond
   ;; type annotation
   ((and (or (eq (cadr comment-form) ::)
             (eq (nth 2 comment-form) ::)))
    (let ((annotation-name (car comment-form))
          (form-name (and (elsa-form-sequence-p reader-form)
                          (elsa-get-name (cadr (elsa-form-sequence reader-form))))))
      (cond
       ((and (elsa-form-sequence-p reader-form)
             (or (elsa-form-function-call-p reader-form 'defun)
                 (elsa-form-function-call-p reader-form 'defsubst)
                 (elsa-form-function-call-p reader-form 'cl-defmethod)
                 (elsa-form-function-call-p reader-form 'cl-defgeneric)))
        (when (and state (not (eq form-name annotation-name)))
          (elsa-state-add-message state
            (elsa-make-warning (elsa-nth 1 reader-form)
              "The function name `%s' and the annotation name `%s' do not match"
              (symbol-name form-name)
              (symbol-name annotation-name))))
        (elsa-state-add-defun state
          (elsa-defun :name (elsa-get-name (cadr (oref reader-form sequence)))
                      :type (cond
                             ((symbolp (nth 2 comment-form))
                              (eval `(elsa-make-type (function () ,@(cddr comment-form)))))
                             (t (eval `(elsa-make-type ,@(cddr comment-form))))))))
       ((and (elsa-form-sequence-p reader-form)
             (or (elsa-form-function-call-p reader-form 'defvar)
                 (elsa-form-function-call-p reader-form 'defcustom)
                 (elsa-form-function-call-p reader-form 'defconst)
                 (eq (car comment-form) 'defvar)))
        (when (eq (car comment-form) 'defvar)
          (!cdr comment-form))
        (when (and state (not (eq form-name annotation-name)))
          (elsa-state-add-message state
            (elsa-make-warning (elsa-nth 1 reader-form)
              "The variable name `%s' and the annotation name `%s' do not match"
              (symbol-name form-name)
              (symbol-name annotation-name))))
        (elsa-state-add-defvar state
          (elsa-defvar :name (elsa-get-name (cadr (oref reader-form sequence)))
                       :type (eval `(elsa-make-type ,@(cddr comment-form))))))
       ((eq annotation-name 'var)
        (oset reader-form annotation comment-form))
       (t
        ;; annotation which is not on defun or defvar will be assumed
        ;; to annotate a variable
        )
       )))))

(defun elsa--set-line-and-column (form)
  "Set line and column properties of form.

Each form must have start and end properties supplied in
constructor.  These are used to derive line and column."
  (oset form line
        (or (get-text-property (save-excursion
                                 (goto-char (oref form start))
                                 (line-beginning-position))
                               'elsa-line)
            (line-number-at-pos (oref form start))))
  (oset form end-line
        (or (get-text-property (save-excursion
                                 (goto-char (oref form end))
                                 (line-beginning-position))
                               'elsa-line)
            (line-number-at-pos (oref form start))))
  (oset form column (save-excursion
                      (goto-char (oref form start))
                      (current-column)))
  (oset form end-column (save-excursion
                          (goto-char (oref form end))
                          (current-column)))
  form)

(defun elsa--read-form (form state)
  "Read FORM.

FORM is a lisp object that was produced by calling `read'.  This
function walks the buffer according to the structure of the form
and produces an object of type `elsa-form' which serves as input
for the analysis."
  (let ((reader-form
         (cond
          ((keywordp form) (elsa--read-keyword form))
          ((symbolp form) (elsa--read-symbol form))
          ((consp form)
           ;; special care needs to be taken about the "reader macros" '`,
           (let ((cons-form (cond
                             ((elsa--quote-p (car form))
                              (elsa--read-quote form state))
                             (t (elsa--read-cons form state)))))
             (elsa-form-foreach cons-form
               (lambda (f) (oset f parent cons-form)))
             cons-form))
          ((integerp form) (elsa--read-integer form))
          ((floatp form) (elsa--read-float form))
          ((stringp form) (elsa--read-string form))
          ((vectorp form)
           (let ((vector-form (elsa--read-vector form state)))
             (elsa-form-foreach vector-form
               (lambda (f) (oset f parent vector-form)))
             vector-form))
          ((functionp form) (elsa--read-function form state))
          (t (error "Invalid form")))))
    (elsa--set-line-and-column reader-form)
    ;; check if there is a comment atached to this form
    ;; TODO: this is really inefficient because it checks the same
    ;; line multiple times.  We should only do this parsing for the
    ;; first form on a line.
    (save-excursion
      (goto-char (oref reader-form start))
      (forward-line -1)
      (skip-chars-forward " \t\n\r")
      (let ((line-end (line-end-position)))
        (when (and (re-search-forward
                    (rx "(" (+ (+? (or (syntax word) (syntax symbol))) (+ space)) ":: ")
                    line-end t)
                   (nth 4 (syntax-ppss)))
          ;; we are inside a comment and inside a form starting with
          ;; (elsa
          (search-backward "(")
          (let ((comment-form (read (current-buffer))))
            ;; we must end on the same line, that way we can be sure
            ;; the entire read form was inside a comment.
            (when (<= (point) line-end)
              ;; handle defun type declaration
              (elsa--process-annotation reader-form comment-form state))))))
    reader-form))

(defun elsa-read-form (state)
  "Read form at point."
  (while (forward-comment 1))
  (unless (eobp)
    (let* ((form (save-excursion
                   (read (current-buffer)))))
      (while (forward-comment 1))
      (elsa--read-form form state))))

(provide 'elsa-reader)
