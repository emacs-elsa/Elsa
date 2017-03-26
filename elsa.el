;;; elsa.el --- Emacs Lisp Static Analyser -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 23rd March 2017
;; Keywords: languages, lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'eieio)

(require 'dash)

(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-defun)
(require 'elsa-error)

(push '(elsa-args (lambda (&rest _) t)) defun-declarations-alist)
(push '(elsa-return (lambda (&rest _) t)) defun-declarations-alist)

(cl-defmacro elsa-cast (_type &rest forms)
  "Assign TYPE to FORMS"
  (declare (indent 1))
  `(progn ,@forms))

;; TODO: add some methods for looking up variables/defuns, so we don't
;; directly work with the hashtable
(defclass elsa-state nil
  ((defvars :initform (make-hash-table))
   (defuns :initform (make-hash-table))
   (errors :initform nil)
   (scope :initform (elsa-scope ""))))

(defclass elsa-defvar nil
  ((name :initarg :name)
   (type :initarg :type)))

(defclass elsa-expression nil
  ((type :initarg :type)))

;; TODO: take defvar directly? For consistency
(defmethod elsa-state-add-defvar ((this elsa-state) name type)
  (let ((defvars (oref this defvars)))
    (puthash name (elsa-defvar "" :name name :type type) defvars)))

(defmethod elsa-state-add-defun ((this elsa-state) defun)
  (unless (elsa-defun-p defun) (error "defun must be `elsa-defun-p'"))
  (let ((defuns (oref this defuns)))
    (puthash (oref defun name) defun defuns)))

(defmethod elsa-state-add-error ((this elsa-state) error)
  (oset this errors (cons error (oref this errors))))

(defun elsa-process-file (file)
  "Process FILE."
  (let ((buffer (find-file-noselect file))
        (state (elsa-state ""))
        (form))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (condition-case _err
              (while (setq form (read buffer))
                (elsa-analyse-form state form))
            (end-of-file t)))))
    (oset state errors (nreverse (oref state errors)))
    state))

(defun elsa--get-expression-type (state expr)
  "Get type of expression EXPR in STATE.

DEFVARS contains the globally defined variables."
  (let ((defvars (oref state defvars))
        (defuns (oref state defuns))
        (scope (oref state scope)))
    (cond
     ((and (consp expr)
           (gethash (car expr) defuns))
      (oref (gethash (car expr) defuns) return-type))
     ((and (symbolp expr)
           (elsa-scope-get-var scope expr))
      (oref (elsa-scope-get-var scope expr) type))
     ((and (symbolp expr)
           (gethash expr defvars))
      (oref (gethash expr defvars) type))
     ;; if it is a symbol and it is not local variable or defvar, then
     ;; it is not bound and therefore an error -> unbound type
     ((and (symbolp expr)
           (not (eq expr nil))
           (not (eq expr t))
           (not (keywordp expr)))
      ;; TODO: should we mutate the state here with an error???
      (elsa-state-add-error
       state (elsa-error
              "" :message (format "Unbound variable `%s'" (symbol-name expr))
              :line (line-number-at-pos)))
      (elsa-make-type 'unbound))
     ((stringp expr)
      (elsa-make-type 'string))
     ((integerp expr)
      (elsa-make-type 'int))
     ((eq expr nil)
      (elsa-make-type nil))
     (t (elsa-make-type 'mixed)))))

(defun elsa-check-function (state types args)
  "Check if function accepts correct arguments.
STATE, TYPES, ARGS"
  (-each args
    (-lambda (arg)
      (-when-let (type (cdar types))
        (let ((other (elsa--get-expression-type state arg)))
          (cond
           ((not (elsa-type-accept type other))
            (elsa-state-add-error
             state
             (elsa-error
              ""
              :message (format "Invalid type, has %s, expected %s"
                               (elsa-type-describe other)
                               (elsa-type-describe type))
              :line (line-number-at-pos)))))))
      ;; TODO: handle optional and keyword args
      (!cdr types))))

(defun elsa-analyse-function-call (state form)
  "Analyse function call FORM in STATE."
  (let ((defuns (oref state defuns)))
    (-when-let (def (gethash (car form) defuns))
      (elsa-check-function state (oref def args) (cdr form))))
  ;; TODO: map recursively on arguments
  (-each (cdr form) (lambda (f) (elsa-analyse-form state f))))

(defun elsa-analyse-defun (state name args body &optional declarations)
  "Add function to STATE under NAME.

ARGS is the arglist which might be enriched with types provided
in DECLARATIONS (or mixed type by default).

BODY is the body of the function which is further analysed."
  (elsa-state-add-defun
   state (elsa-make-defun name args declarations)))

(defun elsa-analyse-let (state bindings body)
  "Analyse a `let' form.
STATE, BINDINGS, BODY."
  (let ((scope (oref state scope))
        (new-vars nil))
    (-each bindings
      (lambda (binding)
        (pcase binding
          (`(,var ,form)
           (push (elsa-variable
                  "" :name var :type (elsa--get-expression-type
                                      state form))
                 new-vars))
          (var
           (push (elsa-variable
                  "" :name var :type (elsa-make-type 'nil))
                 new-vars)))))
    (-each new-vars (lambda (v) (elsa-scope-add-variable scope v)))
    (-each body (lambda (f) (elsa-analyse-form state f)))
    ;; TODO: this is wrong if nested let rebound a variable, we need to "pop!"
    (-each new-vars (lambda (v) (elsa-scope-remove-variable scope v)))))

(defun elsa-analyse-let* (state bindings body)
  "Analyse a `let*' form.
STATE, BINDINGS, BODY."
  (let ((scope (oref state scope))
        (new-vars nil))
    (-each bindings
      (lambda (binding)
        (let ((variable
               (pcase binding
                 (`(,var ,form)
                  (elsa-variable
                   "" :name var :type (elsa--get-expression-type
                                       state form)))
                 (var (elsa-variable "" :name var :type (elsa-make-type 'nil))))))
          (elsa-scope-add-variable scope variable)
          (push variable new-vars))))
    (-each body (lambda (f) (elsa-analyse-form state f)))
    ;; TODO: this is wrong if nested let rebound a variable, we need to "pop!"
    (-each new-vars (lambda (v) (elsa-scope-remove-variable scope v)))))

(defun elsa-analyse-symbol (state symbol)

  )

;; There are multiple tasks we need to do:
;; - crawl the forms and build up scope
;; - figure out types of expressions (and store those?)
;; - run the checkers at each "node" (this should be pluggable and
;;   dynamic)
;; - update global state with defuns/defvars
(defun elsa-analyse-form (state form &optional type)
  "Analyse FORM in STATE.

If TYPE is non-nil, force this type on FORM."
  (pcase form
    (`(elsa-cast ,type ,form)
     (elsa-analyse-form state form (elsa-make-type type)))
    ;; TODO: handle type
    (`(defvar ,name . ,_)
     (elsa-state-add-defvar state name (or type (elsa-type-mixed ""))))
    ;; TODO: handle type
    (`(defcustom ,name ,_ ,(pred stringp) . ,_)
     (elsa-state-add-defvar state name (or type (elsa-type-mixed ""))))
    ;; TODO: handle type
    (`(defcustom ,name ,_ . ,_)
     (elsa-state-add-defvar state name (or type (elsa-type-mixed ""))))
    ;; TODO: add type inference to get return type from exit forms
    (`(defun ,name ,args ,(pred stringp) (declare . ,declarations) . ,body)
     (elsa-analyse-defun state name args body declarations))
    (`(defun ,name ,args ,(pred stringp) . ,body)
     (elsa-analyse-defun state name args body))
    (`(defun ,name ,args . ,body)
     (elsa-analyse-defun state name args body))
    (`(let ,bindings . ,body)
     (elsa-analyse-let state bindings body))
    (`(let* ,bindings . ,body)
     (elsa-analyse-let* state bindings body))
    (`(,function-name . ,rest)
     (elsa-analyse-function-call state (cons function-name rest)))
    ((and (pred symbolp) symbol)
     (elsa-analyse-symbol state symbol))))

(provide 'elsa)
;;; elsa.el ends here
