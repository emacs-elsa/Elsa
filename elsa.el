;;; elsa.el --- Emacs Lisp Static Analyser -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 23rd March 2017
;; Package-requires: ((dash "2.10.0"))
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
;; (require 'elsa-scope)

(cl-defmacro elsa-type (type &rest forms)
  "Assign TYPE to FORMS"
  (declare (indent 1))
  `(progn ,@forms))

(defclass elsa-state nil
  ((defvars :initform (make-hash-table))
   (defuns :initform (make-hash-table))
   (errors :initform nil)))

(defclass elsa-defvar nil
  ((name :initarg :name)
   (type :initarg :type)))

(defclass elsa-defun nil
  ((name :initarg :name)
   (return-type :initarg :return-type)
   (arg-types :initarg :arg-types)))

(defmethod elsa-state-add-defvar ((this elsa-state) name type)
  (let ((defvars (oref this defvars)))
    (puthash name (elsa-defvar "" :name name :type type) defvars)))

(defun elsa-type-definition-to-type (definition)
  "Return instance of class representing DEFINITION."
  (let* ((name (split-string (symbol-name definition) "?"))
         (class (intern (concat "elsa-type-"  (car name)))))
    (make-instance class :nullable (equal (cadr name) ""))))

(defun elsa-defun-get-return-type (declarations)
  "Get return type from DECLARATIONS."
  (-when-let (return-type (cadr (--first (eq (car it) 'elsa-return) declarations)))
    (elsa-type-definition-to-type return-type)))

(defun elsa-defun-get-arg-types (declarations)
  "Get return type from DECLARATIONS."
  (-when-let (arg-types (cdr (--first (eq (car it) 'elsa-args) declarations)))
    (-map
     (lambda (type)
       (elsa-type-definition-to-type type))
     arg-types)))

(defmethod elsa-state-add-defun ((this elsa-state) name declarations)
  (let ((defuns (oref this defuns)))
    (puthash
     name
     (elsa-defun
      ""
      :name name
      :return-type (elsa-defun-get-return-type declarations)
      :arg-types (elsa-defun-get-arg-types declarations))
     defuns)
    ;; (elsa-defun-get-arg-types declarations)
    ))

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

(defun elsa-check-function (arg-types args state)
  "Check if function accepts correct arguments.
ARG-TYPES, ARGS, STATE."
  (let ((defvars (oref state defvars)))
    (-map
     (-lambda ((type . arg))
       (let ((other
              (cond
               ((and (symbolp arg)
                     (gethash arg defvars))
                (let ((defv (gethash arg defvars)))
                  (oref defv type)))
               ((stringp arg)
                (elsa-type-string ""))
               (t arg))))
         (cond
          ((not (elsa-type-child-p other))
           (elsa-state-add-error
            state
            (format "Unknown type, has %S, expected %s"
                    other (elsa-type-describe type))))
          ((not (elsa-type-accept type other))
           (elsa-state-add-error
            state
            (format "Invalid type, has %s, expected %s"
                    (elsa-type-describe other)
                    (elsa-type-describe type)))))))
     (-zip arg-types args))))

(defun elsa-analyse-function-call (form state)
  "Analyse function call FORM in STATE."
  (let ((defuns (oref state defuns)))
    (-when-let (def (gethash (car form) defuns))
      (elsa-check-function (oref def arg-types) (cdr form) state))))

(defun elsa-analyse-form (form state &optional type)
  "Analyse FORM in STATE.

If TYPE is non-nil, force this type on FORM."
  (pcase form
    (`(elsa-type ,type ,form)
     (elsa-analyse-form form state (elsa-type-definition-to-type type)))
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
    (`(defun ,name ,_ ,(pred stringp) (declare . ,declarations) . ,_)
     (elsa-state-add-defun state name declarations))
    (`(,function-name . ,rest)
     (elsa-analyse-function-call (cons function-name rest) state))))

(provide 'elsa)
;;; elsa.el ends here
