;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'pcase)
(require 'backquote)

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

(defun elsa--forward-sexp (&optional n)
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
   (parent :type (or elsa-form nil) :initarg :parent))
  :abstract t)

(cl-defmethod elsa-form-length ((this elsa-form))
  (- (oref this end) (oref this start)))

;;; Atoms
(defclass elsa-form-atom (elsa-form)
  nil
  :abstract t)

(defclass elsa-form-symbol (elsa-form-atom)
  ((name :type symbol :initarg :name)))

(cl-defmethod elsa-form-sequence ((this elsa-form-symbol))
  (if (eq (elsa-form-name this) 'nil)
      nil
    (error "Can not get sequence out of symbol form.")))

(defun elsa--read-symbol (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-symbol
   :start (point)
   :name form
   :end (elsa--forward-sexp)))

(defclass elsa-form-keyword (elsa-form-symbol) ())

(cl-defgeneric elsa-form-function-call-p (this &optional name) nil)

(cl-defgeneric elsa-form-name (this) nil)

(cl-defmethod elsa-form-name ((this elsa-form-symbol))
  (oref this name))

(defun elsa--read-keyword (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-keyword
   :type (elsa-make-type Keyword)
   :start (point)
   :name form
   :end (elsa--forward-sexp)))

(defclass elsa-form-number (elsa-form-atom)
  ((value :type number :initarg :value)))

(defclass elsa-form-integer (elsa-form-number)
  ((value :initarg :value)))

(defun elsa--read-integer (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-integer
   :type (elsa-make-type Int)
   :start (point)
   :value form
   :end (elsa--forward-sexp)))

(defclass elsa-form-float (elsa-form-number)
  ((value :initarg :value)))

(defun elsa--read-float (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-float
   :type (elsa-make-type Float)
   :start (point)
   :value form
   :end (elsa--forward-sexp)))

;;;; Sequences
(defclass elsa-form-seq (elsa-form-atom)
  nil
  :abstract t)

(defclass elsa-form-string (elsa-form-seq)
  ((sequence :type string :initarg :sequence)))

(defun elsa--read-string (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-string
   :type (elsa-make-type String)
   :start (point)
   :end (elsa--forward-sexp)
   :sequence form))

(defclass elsa-form-vector (elsa-form-seq)
  ((sequence :type vector :initarg :sequence)))

(defun elsa--read-vector (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-vector
   :type (elsa-make-type Vector)
   :start (prog1 (point) (down-list))
   :sequence (apply 'vector (-map 'elsa--read-form form))
   :end (progn (up-list) (point))))

;;; Conses
(defclass elsa-form-cons (elsa-form) ())

(defclass elsa-form-list (elsa-form-cons elsa-form-seq)
  ((sequence :type list :initarg :sequence)))

(cl-defmethod elsa-form-car ((this elsa-form-list))
  (car (oref this sequence)))

(cl-defmethod elsa-form-cdr ((this elsa-form-list))
  (cdr (oref this sequence)))

(cl-defmethod elsa-form-sequence ((this elsa-form-list))
  (oref this sequence))

(cl-defmethod elsa-form-name ((this elsa-form-list))
  (-when-let (head (elsa-form-car this))
    (and (elsa-form-symbol-p head)
         (oref head name))))

(cl-defmethod elsa-form-function-call-p ((this elsa-form-list) &optional name)
  (-when-let (head (elsa-form-car this))
    (and (elsa-form-symbol-p head)
         (or (not name)
             (eq (oref head name) name)))))

(defclass elsa-form-improper-list (elsa-form-cons)
  ((conses :type list :initarg :conses)))

(cl-defmethod elsa-form-car ((this elsa-form-improper-list))
  (car (oref this conses)))

(cl-defmethod elsa-form-cdr ((this elsa-form-improper-list))
  (cdr (oref this conses)))

;; (elsa :: [Mixed] -> Mixed)
(defun elsa--read-cons (form)
  (elsa--skip-whitespace-forward)
  (if (elsa--improper-list-p form)
      (elsa-form-improper-list
       :type (elsa-make-type List)
       :start (prog1 (point) (down-list))
       :conses (let ((depth 0)
                     (items))
                 (while (consp form)
                   (let ((head (elsa--read-form (car form))))
                     (push head items)
                     (!cdr form)
                     (elsa--skip-whitespace-forward)
                     (when (looking-at-p "\\.")
                       (if (consp form)
                           (progn
                             (cl-incf depth)
                             (down-list))
                         (push (elsa--read-form form) items)))))
                 (while (>= (cl-decf depth) 0) (up-list))
                 (apply 'cl-list* (nreverse items)))
        :end (progn (up-list) (point)))
    (elsa-form-list
     :start (prog1 (point) (down-list))
     :sequence
      (let ((depth 0)
            (items))
        (while form
          (let ((head (elsa--read-form (car form))))
            (push head items)
            (!cdr form)
            (elsa--skip-whitespace-forward)
            (when (and form (looking-at-p "\\."))
              (cl-incf depth)
              (down-list))))
        (while (>= (cl-decf depth) 0) (up-list))
        (nreverse items))
      :end (progn (up-list) (point)))))

(defun elsa--read-quote (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-list
   :quote-type (car form)
   :start (point)
   :sequence (cons
              (elsa-form-symbol
               :start (prog1 (point)
                        (if (looking-at-p "#?'")
                            (skip-syntax-forward "'")
                          (forward-char (length (symbol-name (car form))))))
               :name (car form)
               :end (point))
              (-map 'elsa--read-form (cdr form)))
   :end (point)))

(defun elsa--read-form (form)
  (let ((reader-form
         (cond
          ((floatp form) (elsa--read-float form))
          ((integerp form) (elsa--read-integer form))
          ((keywordp form) (elsa--read-keyword form))
          ((symbolp form) (elsa--read-symbol form))
          ((vectorp form) (elsa--read-vector form))
          ((stringp form) (elsa--read-string form))
          ((consp form)
           ;; special care needs to be taken about the "reader macros" '`,
           (cond
            ((memq (car form) (list 'function 'quote backquote-backquote-symbol backquote-unquote-symbol backquote-splice-symbol))
             (elsa--read-quote form))
            (t (elsa--read-cons form))))
          (t (error "Invalid form")))))
    (oset reader-form line (line-number-at-pos (oref reader-form start)))
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
        (when (and (search-forward "(elsa" line-end t)
                   (nth 4 (syntax-ppss)))
          ;; we are inside a comment and inside a form starting with
          ;; (elsa
          (search-backward "(elsa")
          (let ((comment-form (read (current-buffer))))
            ;; we must end on the same line, that way we can be sure
            ;; the entire read form was inside a comment.
            (when (<= (point) line-end)
              ;; handle defun type declaration
              (elsa--process-annotation reader-form comment-form))))))
    reader-form))

(defun elsa--process-annotation (reader-form comment-form)
  (cond
   ((and (elsa-form-function-call-p reader-form 'defun)
         (eq (car comment-form) 'elsa)
         (eq (cadr comment-form) ::))
    (put (elsa-form-name (cadr (oref reader-form sequence)))
         'elsa-type
         ;; TODO: get rid of eval
         (eval `(elsa-make-type ,@(cddr comment-form)))))))

(defun elsa-read-form ()
  "Read form at point."
  (let* ((form (save-excursion
                 (read (current-buffer)))))
    (elsa--read-form form)))

(provide 'elsa-reader)
