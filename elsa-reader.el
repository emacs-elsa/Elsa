;; -*- lexical-binding: t -*-
(require 'eieio)
(require 'pcase)

(defun elsa--skip-whitespace-forward ()
  (skip-chars-forward " \t\n\r"))

(defclass elsa-form nil
  ((start :type integer :initarg :start)
   (end :type integer :initarg :end)
   (type :type elsa-type :initarg :type))
  :abstract t)

(cl-defmethod elsa-form-length ((this elsa-form))
  (- (oref this end) (oref this start)))

(defclass elsa-form-symbol (elsa-form)
  ((name :type symbol :initarg :name)))

(defclass elsa-form-keyword (elsa-form-symbol) ())

(defclass elsa-form-integer (elsa-form)
  ((value :type integer :initarg :value)))

(defclass elsa-form-string (elsa-form)
  ((content :type string :initarg :content)))

(defclass elsa-form-quote (elsa-form)
  ((content :initarg :content)))

(defclass elsa-form-function-call (elsa-form)
  ((name :type elsa-form-symbol :initarg :name)
   (args :type list :initarg :args)))

(defclass elsa-form-body (elsa-form)
  ((body :type list :initarg :body))
  :documentation
  "Implicit body/progn.")

(defclass elsa-form-progn (elsa-form-body) ())

(defclass elsa-form-if (elsa-form)
  ((condition :type elsa-form :initarg :condition)
   (then :type elsa-form :initarg :then)
   (else :type list :initarg :else)))

(defclass elsa-form-let (elsa-form)
  ((bindings :type list :initarg :bindings)
   (body :type list :initarg :body)))

(defclass elsa-form-let* (elsa-form-let)
  ((bindings :type list :initarg :bindings)
   (body :type list :initarg :body)))

(defclass elsa-form-function-argument (elsa-form)
  ((name :type symbol :initarg :name)))

(defclass elsa-form-function-argument-list (elsa-form)
  ((args :type list :initarg :args)))

(defclass elsa-form-lambda (elsa-form)
  ((args :type elsa-form-function-argument-list :initarg :args)
   (docstring :type (or elsa-form-string null) :initarg :docstring)
   (body :type list :initarg :body)))

(cl-defmethod elsa-form-defun-args ((this elsa-form-lambda))
  (oref (oref this args) args))

(cl-defmethod elsa-form-defun-docstring ((this elsa-form-lambda))
  (--when-let (oref this docstring) (oref it content)))

(defclass elsa-form-defun (elsa-form-lambda)
  ((name :type elsa-form-symbol :initarg :name)))

(cl-defmethod elsa-form-defun-name ((this elsa-form-defun))
  (oref (oref this name) name))

(defun elsa--read-defun (name args docstring body)
  (elsa--skip-whitespace-forward)
  (let* ((start (prog1 (point)
                  (forward-symbol 1)))
         (name-form (elsa--read-symbol name))
         (arg-list-form (elsa--read-argument-list-declaration args))
         (docstring-form (when docstring
                           (elsa--read-string docstring)))
         (body-form (-map 'elsa--read-form body)))
    (up-list)
    (elsa-form-defun
     :start start
     :end (point)
     :name name-form
     :args arg-list-form
     :docstring docstring-form
     :body body-form)))

(defun elsa--read-lambda (args docstring body)
  (elsa--skip-whitespace-forward)
  (let* ((start (prog1 (point)
                  (forward-symbol 1)))
         (arg-list-form (elsa--read-argument-list-declaration args))
         (docstring-form (when docstring
                           (elsa--read-string docstring)))
         (body-form (-map 'elsa--read-form body)))
    (up-list)
    (elsa-form-lambda
     :start start
     :end (point)
     :args arg-list-form
     :docstring docstring-form
     :body body-form)))

(defun elsa--read-argument-list-declaration (args)
  (elsa--skip-whitespace-forward)
  (elsa-form-function-argument-list
   :start (prog1 (point) (down-list))
   :args (-map 'elsa--read-symbol args)
   :end (progn (up-list) (point))))

(defun elsa--read-progn (body)
  (elsa--skip-whitespace-forward)
  (elsa-form-progn
   :start (prog1 (point) (forward-symbol 1))
   :body (-map 'elsa--read-form body)
   :end (progn (up-list) (point))))

(defun elsa--read-if (condition then else)
  (elsa--skip-whitespace-forward)
  (elsa-form-if
   :start (prog1 (point) (forward-symbol 1))
   :condition (elsa--read-form condition)
   :then (elsa--read-form then)
   :else (-map 'elsa--read-form else)
   :end (progn (up-list) (point))))

(defun elsa---read-let (constructor bindings body)
  (elsa--skip-whitespace-forward)
  (funcall
   constructor
   :start (prog1 (point) (forward-symbol 1))
   :bindings
    (if (= (length bindings) 0)
        (forward-sexp)
      (down-list)
      (prog1 (-map
              (lambda (binding)
                (pcase binding
                  (`(,var ,form)
                   (down-list)
                   (prog1 (list (elsa--read-symbol var) (elsa--read-form form))
                     (up-list)))
                  (var
                   (elsa--read-symbol var))))
              bindings)
        (up-list)))
    :body (-map 'elsa--read-form body)
    :end (progn (up-list) (point))))

(defun elsa--read-let (bindings body)
  (elsa---read-let 'elsa-form-let bindings body))

(defun elsa--read-let* (bindings body)
  (elsa---read-let 'elsa-form-let* bindings body))

(defun elsa--read-quote (x)
  (elsa--skip-whitespace-forward)
  (elsa-form-quote
   :start (point)
   :content x
   :end (progn (forward-sexp) (point))))

(defun elsa--read-function-call (form)
  (elsa--skip-whitespace-forward)
  (pcase form
    (`(defun ,name ,args ,(and (pred stringp) docstring) . ,body)
     (elsa--read-defun name args docstring body))
    (`(defun ,name ,args . ,body)
     (elsa--read-defun name args nil body))
    (`(lambda ,args ,(and (pred stringp) docstring) . ,body)
     (elsa--read-lambda args docstring body))
    (`(lambda ,args . ,body)
     (elsa--read-lambda args nil body))
    (`(progn . ,body)
     (elsa--read-progn body))
    (`(if ,condition ,then . ,else)
     (elsa--read-if condition then else))
    (`(let ,bindings . ,body)
     (elsa--read-let bindings body))
    (`(let* ,bindings . ,body)
     (elsa--read-let* bindings body))
    (`(quote ,x)
     (elsa--read-quote x))
    (t
     (let* ((start (prog1 (point)
                     (down-list)))
            (name (elsa--read-symbol (car form)))
            (args (-map 'elsa--read-form (cdr form))))
       (up-list)
       (elsa-form-function-call
        :start start :end (point)
        :name name :args args)))))

(defun elsa--read-string (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-string
   :start (point)
   :end (progn (forward-sexp) (point))
   :content form))

;; turn this into a macro for symbol/keyword/itneger and other atoms
(defun elsa--read-symbol (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-symbol
   :start (point)
   :name form
   :end (progn
          (forward-symbol 1)
          (point))))

(defun elsa--read-keyword (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-keyword
   :start (point)
   :name form
   :end (progn
          (forward-symbol 1)
          (point))))

(defun elsa--read-integer (form)
  (elsa--skip-whitespace-forward)
  (elsa-form-integer
   :start (point)
   :value form
   :end (progn
          (forward-symbol 1)
          (point))))

(defun elsa--read-form (form)
  (cond
   ((consp form) (elsa--read-function-call form))
   ((stringp form) (elsa--read-string form))
   ((keywordp form) (elsa--read-keyword form))
   ((integerp form) (elsa--read-integer form))
   (t (elsa--read-symbol form))))

(defun elsa-read-form ()
  "Read form at point."
  (let* ((form (save-excursion
                 (read (current-buffer)))))
    (elsa--read-form form)))

(provide 'elsa-reader)
