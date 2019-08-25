;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'pcase)
(require 'backquote)

(require 'trinary)

(require 'elsa-error)
(require 'elsa-types)
(require 'elsa-type-helpers)

(defun elsa--skip-whitespace-forward ()
  (skip-chars-forward " \t\n\r"))

(defun elsa--improper-list-p (cell)
  "Return non-nil if CELL is an improper list."
  (let ((len (safe-length cell)))
    (when (and (consp cell)
               (> len 0)
               (not (listp (nthcdr len cell))))
      (nthcdr len cell))))

;; TODO: this will be easier to do later when we properly push quoted
;; types up.  The check will then be just for a symbol and a quote
(defun elsa--quoted-symbol-p (form)
  "Return non-nil if FORM represents a quoted symbol."
  (when (eq (oref form quote-type) 'quote)
    (-when-let (seq (elsa-form-sequence form))
      (when (= (length seq) 2)
        (elsa-form-symbol-p (cadr seq))))))

(defun elsa--quoted-symbol-name (form)
  "Return the name of quoted symbol that FORM represents.

Nil if FORM is not a quoted symbol."
  (when (elsa--quoted-symbol-p form)
    (elsa-get-name (cadr (elsa-form-sequence form)))))

(defsubst elsa--quote-p (symbol)
  "Return non-nil if SYMBOL is a type of quote."
  (memq symbol (list
                'function
                'quote
                backquote-backquote-symbol
                backquote-unquote-symbol
                backquote-splice-symbol)))

;; (elsa--forward-sexp :: Int? -> Int)
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
   (type :type elsa-type :initarg :type :initform (elsa-make-type Mixed))
   (narrow-types :initarg :narrow-type :initform nil)
   (reachable :type trinary :initarg :reachable :initform (trinary-true))
   (parent :type (or elsa-form nil) :initarg :parent))
  :abstract t)

;; (elsa-get-name :: Mixed -> Symbol?)
(cl-defgeneric elsa-get-name (_this) nil)

;; (elsa-form-sequence :: Mixed -> [Mixed])
(cl-defgeneric elsa-form-sequence (form)
  "Return the sequence of things contained in FORM.")

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

(cl-defmethod elsa-type-accept ((this elsa-type) (other elsa-form))
  (elsa-type-accept this (oref other type)))

(cl-defmethod elsa-type-accept ((this elsa-form) (other elsa-type))
  (elsa-type-accept (oref this type) other))

(cl-defmethod elsa-type-is-nil ((condition elsa-form))
  (elsa-type-is-nil (oref condition type)))

(cl-defmethod elsa-type-is-non-nil ((condition elsa-form))
  (elsa-type-is-non-nil (oref condition type)))

(cl-defmethod elsa-form-print ((_this elsa-form)) "")

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

;;; Atoms
(defclass elsa-form-atom (elsa-form)
  nil
  :abstract t)

(defclass elsa-form-symbol (elsa-form-atom)
  ((name :type symbol :initarg :name)))

(cl-defmethod elsa-form-print ((this elsa-form-symbol))
  (symbol-name (oref this name)))

(cl-defmethod elsa-form-sequence ((this elsa-form-symbol))
  (if (eq (elsa-get-name this) 'nil)
      nil
    (error "Can not get sequence out of symbol form")))

(cl-defgeneric elsa-form-sequence-p (this)
  nil)

(cl-defmethod elsa-form-sequence-p ((this elsa-form-symbol))
  (eq (elsa-get-name this) 'nil))

(defsubst elsa--read-symbol (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-symbol
   :start (point)
   :name form
   :end (elsa--forward-sexp)))

(defclass elsa-form-keyword (elsa-form-symbol) ())

(cl-defmethod elsa-form-print ((this elsa-form-keyword))
  (symbol-name (oref this name)))

;; (elsa-form-function-call-p :: Mixed -> Symbol? -> Bool)
(cl-defgeneric elsa-form-function-call-p (_this &optional _name) nil)

(cl-defmethod elsa-get-name ((this elsa-form-symbol))
  (oref this name))

(defsubst elsa--read-keyword (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-keyword
   :type (elsa-const-type
          :type (elsa-make-type Keyword)
          :value form)
   :start (point)
   :name form
   :end (elsa--forward-sexp)))

(defclass elsa-form-number (elsa-form-atom)
  ((value :type number :initarg :value)))

(cl-defmethod elsa-form-print ((this elsa-form-number))
  (number-to-string (oref this value)))

(defclass elsa-form-integer (elsa-form-number)
  ((value :initarg :value)))

(defsubst elsa--read-integer (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-integer
   :type (elsa-const-type
          :type (elsa-make-type Int)
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
          :type (elsa-make-type Float)
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
  (oref this sequence))

(defsubst elsa--read-string (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-string
   :type (elsa-const-type
          :type (elsa-make-type String)
          :value form)
   :start (point)
   :end (elsa--forward-sexp)
   :sequence form))

(defclass elsa-form-vector (elsa-form-seq)
  ((sequence :type list :initarg :sequence)))

(cl-defmethod elsa-form-print ((this elsa-form-vector))
  (format "[%s]" (mapconcat 'elsa-form-print (oref this sequence) " ")))

(cl-defmethod elsa-form-foreach ((this elsa-form-vector) fn)
  (mapc fn (oref this sequence)))

(cl-defmethod elsa-form-visit ((this elsa-form-vector) fn)
  (elsa-form-foreach this (lambda (x) (elsa-form-visit x fn))))

(cl-defmethod elsa-form-sequence ((this elsa-form-vector))
  (oref this sequence))

(defsubst elsa--read-vector (form state)
  (elsa--skip-whitespace-forward)
  (elsa-form-vector
   :type (elsa-make-type Vector)
   :start (prog1 (point) (down-list))
   :sequence (-map (lambda (f) (elsa--read-form f state)) form)
   :end (progn (up-list) (point))))

;;; Conses
(defclass elsa-form-cons (elsa-form) ())

(defclass elsa-form-list (elsa-form-cons elsa-form-seq)
  ((sequence :type list :initarg :sequence)))

(cl-defmethod elsa-form-print ((this elsa-form-list))
  (format "(%s)" (mapconcat 'elsa-form-print (oref this sequence) " ")))

(cl-defmethod elsa-form-foreach ((this elsa-form-list) fn)
  (mapc fn (oref this sequence)))

(cl-defmethod elsa-form-visit ((this elsa-form-list) fn)
  (elsa-form-foreach this (lambda (x) (elsa-form-visit x fn))))

;; (elsa-car :: Mixed -> Mixed)
(cl-defgeneric elsa-car (thing)
  "Return `car' of THING")

(cl-defmethod elsa-car ((this list))
  (car this))

(cl-defmethod elsa-car ((this elsa-form))
  (car (elsa-form-sequence this)))

;; (elsa-cdr :: Mixed -> [Mixed])
(cl-defgeneric elsa-cdr (thing)
  "Return `cdr' of THING")

(cl-defmethod elsa-cdr ((this list))
  (cdr this))

(cl-defmethod elsa-cdr ((this elsa-form))
  (cdr (elsa-form-sequence this)))

;; (elsa-cadr :: Mixed -> Mixed)
(cl-defgeneric elsa-cadr (thing)
  "Return `cadr' of THING")

(cl-defmethod elsa-cadr ((this list))
  (cadr this))

(cl-defmethod elsa-cadr ((this elsa-form))
  (cadr (elsa-form-sequence this)))

;; (elsa-nth :: Int -> Mixed -> Mixed)
(cl-defgeneric elsa-nth (n thing)
  "Return nth item of THING")

(cl-defmethod elsa-nth (n (this list))
  (nth n this))

(cl-defmethod elsa-nth (n (this elsa-form))
  (nth n (elsa-form-sequence this)))

;; (elsa-nthcdr :: Int -> Mixed -> [Mixed])
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
         (prefix (-take len seq))
         (last (cdr (last seq))))
    (format "(%s . %s)"
            (mapconcat 'elsa-form-print prefix " ")
            (elsa-form-print last))))

(cl-defmethod elsa-form-foreach ((this elsa-form-improper-list) fn)
  (let* ((seq (oref this conses))
         (len (safe-length seq))
         (prefix (-take len seq))
         (last (cdr (last seq))))
    (mapc fn (-snoc prefix last))))

(cl-defmethod elsa-form-visit ((this elsa-form-improper-list) fn)
  (elsa-form-foreach this (lambda (x) (elsa-form-visit x fn))))

(cl-defmethod elsa-car ((this elsa-form-improper-list))
  (car (oref this conses)))

(cl-defmethod elsa-cdr ((this elsa-form-improper-list))
  (cdr (oref this conses)))

;; (elsa--read-cons :: [Mixed] -> Mixed -> Mixed)
(defsubst elsa--read-cons (form state)
  (elsa--skip-whitespace-forward)
  (if (elsa--improper-list-p form)
      (elsa-form-improper-list
       :type (elsa-make-type List)
       :start (prog1 (point) (down-list))
       :conses (let ((depth 0)
                     (items))
                 (while (consp form)
                   (let ((head (elsa--read-form (car form) state)))
                     (push head items)
                     (!cdr form)
                     (elsa--skip-whitespace-forward)
                     (when (looking-at-p "\\.")
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
          (when (and form (looking-at-p "\\."))
            (if (elsa--quote-p (car form))
                (forward-sexp) ;; skip the dot
              (cl-incf depth)
              (down-list))))
        (while (>= (cl-decf depth) 0) (up-list))
        (nreverse items))
      :end (progn (up-list) (point)))))

(defun elsa--read-quote (form state)
  (elsa--skip-whitespace-forward)
  (let ((expanded-form nil))
    (elsa-form-list
     :quote-type (car form)
     :start (point)
     :sequence (cons
                (elsa-form-symbol
                 :start (cond
                         ((looking-at (rx (or (and (? "#") "'") "," "`" ",@")))
                          (prog1 (point)
                            (forward-char (length (match-string 0)))))
                         ((looking-at-p "(")
                          (setq expanded-form t)
                          (down-list)
                          (forward-sexp)
                          (point))
                         (t (prog1 (point)
                              (forward-char (length (symbol-name (car form)))))))
                 :name (car form)
                 :end (point))
                (-map (lambda (f) (elsa--read-form f state)) (cdr form)))
     :end (progn
            (when expanded-form (up-list))
            (point)))))

(defsubst elsa--process-annotation (reader-form comment-form state)
  (cond
   ;; type annotation
   ((and (eq (cadr comment-form) ::)
         (elsa-form-sequence-p reader-form))
    (let ((annotation-name (car comment-form))
          (form-name (elsa-get-name (cadr (elsa-form-sequence reader-form)))))
      (cond
       ((or (elsa-form-function-call-p reader-form 'defun)
            (elsa-form-function-call-p reader-form 'defsubst)
            (elsa-form-function-call-p reader-form 'cl-defgeneric))
        (when (and state (not (eq form-name annotation-name)))
          (elsa-state-add-message state
            (elsa-make-warning reader-form
              "The function name `%s' and the annotation name `%s' do not match"
              (symbol-name form-name)
              (symbol-name annotation-name))))
        (elsa-state-add-defun
         state
         (elsa-get-name (cadr (oref reader-form sequence)))
         (eval `(elsa-make-type ,@(cddr comment-form)))))
       ((or (elsa-form-function-call-p reader-form 'defvar)
            (elsa-form-function-call-p reader-form 'defconst))
        (when (and state (not (eq form-name annotation-name)))
          (elsa-state-add-message state
            (elsa-make-warning reader-form
              "The variable name `%s' and the annotation name `%s' do not match"
              (symbol-name form-name)
              (symbol-name annotation-name))))
        (put (elsa-get-name (cadr (oref reader-form sequence)))
             'elsa-type-var
             (eval `(elsa-make-type ,@(cddr comment-form))))))))))

(defun elsa--read-form (form state)
  "Read FORM.

FORM is a lisp object that was produced by calling `read'.  This
function walks the buffer according to the structure of the form
and produces an object of type `elsa-form' which serves as input
for the analysis."
  (let ((reader-form
         (cond
          ((floatp form) (elsa--read-float form))
          ((integerp form) (elsa--read-integer form))
          ((keywordp form) (elsa--read-keyword form))
          ((symbolp form) (elsa--read-symbol form))
          ((vectorp form)
           (let ((vector-form (elsa--read-vector form state)))
             (elsa-form-foreach vector-form
               (lambda (f) (oset f parent vector-form)))
             vector-form))
          ((stringp form) (elsa--read-string form))
          ((consp form)
           ;; special care needs to be taken about the "reader macros" '`,
           (let ((cons-form (cond
                             ((elsa--quote-p (car form))
                              (elsa--read-quote form state))
                             (t (elsa--read-cons form state)))))
             (elsa-form-foreach cons-form
               (lambda (f) (oset f parent cons-form)))
             cons-form))
          (t (error "Invalid form")))))
    (oset reader-form line
          (or (get-text-property (save-excursion
                                   (goto-char (oref reader-form start))
                                   (line-beginning-position))
                                 'elsa-line)
              (line-number-at-pos (oref reader-form start))))
    (oset reader-form column (save-excursion
                               (goto-char (oref reader-form start))
                               (current-column)))
    ;; check if there is a comment atached to this form
    ;; TODO: this is really inefficient because it checks the same
    ;; line multiple times.  We should only do this parsing for the
    ;; first form on a line.
    (save-excursion
      (goto-char (oref reader-form start))
      (forward-line -1)
      (elsa--skip-whitespace-forward)
      (let ((line-end (line-end-position)))
        (when (and (re-search-forward "(.*?::" line-end t)
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
  (let* ((form (save-excursion
                 (read (current-buffer)))))
    (while (forward-comment 1))
    (elsa--read-form form state)))

(provide 'elsa-reader)
