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
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (condition-case err
            (while (setq form (read buffer))
              (elsa-analyse-form form state))
          (end-of-file (message "err %S" err)))))
    (oset state errors (nreverse (oref state errors)))
    state))

(defun elsa--get-type-of-argument (arg defvars)
  "Get type of argument ARG in a function call form.

DEFVARS contains the globally defined variables."
  (cond
   ((and (symbolp arg)
         (gethash arg defvars))
    (oref (gethash arg defvars) type))
   ((stringp arg)
    (elsa-make-type 'string))
   ((integerp arg)
    (elsa-make-type 'int))
   ((eq arg nil)
    (elsa-make-type nil))
   (t (elsa-make-type 'mixed))))

(defun elsa-check-function (state types args)
  "Check if function accepts correct arguments.
STATE, TYPES, ARGS"
  (let ((defvars (oref state defvars)))
    (-each args
      (-lambda (arg)
        (let ((type (cdar types))
              (other (elsa--get-type-of-argument arg defvars)))
          (cond
           ((not (elsa-type-accept type other))
            (elsa-state-add-error
             state
             (elsa-error
              ""
              :message (format "Invalid type, has %s, expected %s"
                               (elsa-type-describe other)
                               (elsa-type-describe type))
              :line (line-number-at-pos))))))
        ;; TODO: handle optional and keyword args
        (!cdr types)))))

(defun elsa-analyse-function-call (state form)
  "Analyse function call FORM in STATE."
  (let ((defuns (oref state defuns)))
    (-when-let (def (gethash (car form) defuns))
      (elsa-check-function state (oref def args) (cdr form)))))

(defun elsa-analyse-defun (state name args body &optional declarations)
  "Add function to STATE under NAME.

ARGS is the arglist which might be enriched with types provided
in DECLARATIONS (or mixed type by default).

BODY is the body of the function which is further analysed."
  (elsa-state-add-defun
   state (elsa-make-defun name args declarations)))

(defun elsa-analyse-form (form state &optional type)
  "Analyse FORM in STATE.

If TYPE is non-nil, force this type on FORM."
  (pcase form
    (`(elsa-cast ,type ,form)
     (elsa-analyse-form form state (elsa-make-type type)))
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
    (`(,function-name . ,rest)
     (elsa-analyse-function-call state (cons function-name rest)))))

(provide 'elsa)
;;; elsa.el ends here
