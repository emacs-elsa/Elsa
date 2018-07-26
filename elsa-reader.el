;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'pcase)
(require 'backquote)

(defun elsa--skip-whitespace-forward ()
  (skip-chars-forward " \t\n\r"))

(defun elsa--forward-symbol (&optional n)
  "Skip `forward-symbol' N times and return `point'."
  (setq n (or n 1))
  (forward-symbol n)
  (point))

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
   (end :type integer :initarg :end))
  :abstract t)

(cl-defmethod elsa-form-length ((this elsa-form))
  (- (oref this end) (oref this start)))

;;; Atoms
(defclass elsa-form-atom (elsa-form)
  nil
  :abstract t)

(defclass elsa-form-symbol (elsa-form-atom)
  ((name :type symbol :initarg :name)))

(defun elsa--read-symbol (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-symbol
   :start (point)
   :name form
   :end (elsa--forward-symbol)))

(defclass elsa-form-keyword (elsa-form-symbol) ())

(defun elsa--read-keyword (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-keyword
   :start (point)
   :name form
   :end (elsa--forward-symbol)))

(defclass elsa-form-number (elsa-form-atom)
  ((value :type number :initarg :value)))

(defclass elsa-form-integer (elsa-form-number)
  ((value :initarg :value)))

(defun elsa--read-integer (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-integer
   :start (point)
   :value form
   :end (elsa--forward-symbol)))

(defclass elsa-form-float (elsa-form-number)
  ((value :initarg :value)))

(defun elsa--read-float (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-float
   :start (point)
   :value form
   :end (elsa--forward-symbol)))

;;;; Sequences
(defclass elsa-form-seq (elsa-form-atom)
  nil
  :abstract t)

(defclass elsa-form-string (elsa-form-seq)
  ((sequence :type string :initarg :sequence)))

(defun elsa--read-string (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-string
   :start (point)
   :end (elsa--forward-sexp)
   :sequence form))

(defclass elsa-form-vector (elsa-form-seq)
  ((sequence :type vector :initarg :sequence)))

(defun elsa--read-vector (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-vector
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

(defclass elsa-form-improper-list (elsa-form-cons)
  ((conses :type list :initarg :conses)))

(cl-defmethod elsa-form-car ((this elsa-form-improper-list))
  (car (oref this conses)))

(cl-defmethod elsa-form-cdr ((this elsa-form-improper-list))
  (cdr (oref this conses)))

(defun elsa--read-cons (form)
  (elsa--skip-whitespace-forward)
  (if (elsa--improper-list-p form)
      (elsa-form-improper-list
       :start (prog1 (point) (down-list))
       :conses (prog1 (nconc (-map 'elsa--read-form
                                   (-take (safe-length form) form))
                             (progn
                               (forward-sexp)
                               (elsa--read-form (cdr (last form))))))
       :end (progn (up-list) (point)))
    (elsa-form-list
     :start (prog1 (point) (down-list))
     :sequence (-map 'elsa--read-form form)
     :end (progn (up-list) (point)))))

(defun elsa--read-quote (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-list
   :start (point)
   :sequence (cons
              (elsa-form-symbol
               :start (prog1 (point) (forward-char))
               :name 'quote
               :end (point))
              (-map 'elsa--read-form (cdr form)))
   :end (point)))

(defun elsa--read-form (form)
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
     ((eq (car form) 'quote)
      (elsa--read-quote form))
     (t (elsa--read-cons form))))
   (t (error "Invalid form"))))

(defun elsa-read-form ()
  "Read form at point."
  (let* ((form (save-excursion
                 (read (current-buffer)))))
    (elsa--read-form form)))

(provide 'elsa-reader)
