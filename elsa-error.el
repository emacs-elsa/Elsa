;;; elsa-error.el --- elsa-error -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 26th March 2017
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
(eval-and-compile (setq eieio-backward-compatibility nil))
(require 'map)

(require 'ansi)
(require 'lsp-protocol)

(require 'elsa-explainer)
(require 'elsa-form)

(defclass elsa-message ()
  ((message
    :type string
    :initarg :message)
   (line
    :type (or integer null)
    :initarg :line
    :initform nil)
   (column
    :type (or integer null)
    :initarg :column
    :initform nil)
   (expression
    :type elsa-form
    :initarg :expression)
   (code
    :type (or string null)
    :initarg :code
    :initform nil))
  :abstract t
  :documentation "Base class representing a message: result of the analysis.

In general, we recognize three states: error, warning, notice

- error = Error is something that is 100% sure to break the
  logic of the program.
- warning = Potential error, we don't know for sure but it's worth
  looking into.
- notice = Maybe you can improve something here, but it does not
  matter for correctness.")

(defclass elsa-error (elsa-message) ())
(defclass elsa-warning (elsa-message) ())
(defclass elsa-notice (elsa-message) ())

(cl-defgeneric elsa-message-type ((this elsa-message))
  "Retrieve human readable description of THIS message type.")

(cl-defmethod elsa-message-type ((_this elsa-message))
  "message")

(cl-defmethod elsa-message-type ((_this elsa-error))
  "error")

(cl-defmethod elsa-message-type ((_this elsa-warning))
  "warning")

(cl-defmethod elsa-message-type ((_this elsa-notice))
  "notice")

(cl-defgeneric elsa-message-type-ansi ((this elsa-message))
  "Retrieve human readable description of THIS message type with ANSI color.")

(cl-defmethod elsa-message-type-ansi ((_this elsa-error))
  (ansi-bright-red "error"))

(cl-defmethod elsa-message-type-ansi ((_this elsa-warning))
  (ansi-bright-yellow "warning"))

(cl-defmethod elsa-message-type-ansi ((_this elsa-notice))
  (ansi-bright-blue "notice"))

(cl-defgeneric elsa-message-to-lsp-severity ((this elsa-message))
  "Retrieve LSP severity code FOR this message.")

(cl-defmethod elsa-message-to-lsp-severity ((_this elsa-error))
  lsp/diagnostic-severity-error)

(cl-defmethod elsa-message-to-lsp-severity ((_this elsa-warning))
  lsp/diagnostic-severity-warning)

(cl-defmethod elsa-message-to-lsp-severity ((_this elsa-notice))
  lsp/diagnostic-severity-information)

(defun elsa--message-resolve (expression)
  (or (when-let ((of (elsa-form-find-parent expression
                       (lambda (x)
                         (oref x original-form)))))
        (oref of original-form))
      expression))

(cl-defmethod elsa-message-format ((this elsa-message))
  "Format an `elsa-message'."
  (let ((expr (elsa--message-resolve (oref this expression))))
    (with-ansi
     (bright-green "%s" (oref expr line))
     ":"
     (green "%s" (or (oref expr column) "?"))
     ":"
     (elsa-message-type-ansi this)
     ":"
     (format "%s" (replace-regexp-in-string "%" "%%" (oref this message))))))

(cl-defmethod elsa-message-to-lsp ((this elsa-message))
  (let ((expression (elsa--message-resolve (oref this expression))))
    (lsp-make-diagnostic
     :code (oref this code)
     :range (lsp-make-range
             :start (lsp-make-position
                     :line (1- (oref expression line))
                     :character (oref expression column))
             :end (lsp-make-position
                   :line (1- (oref expression line))
                   :character (let ((expr expression))
                                (if (and (elsa-form-sequence-p expr)
                                         (elsa-car expr))
                                    (oref (elsa-car expr) end-column)
                                  (+ (oref expression column)
                                     (if (or (elsa-form-symbol-p expr)
                                             (elsa-get-name expr))
                                         (length (symbol-name (elsa-get-name expr)))
                                       1))))))
     :severity (elsa-message-to-lsp-severity this)
     :message (oref this message))))

(defun elsa--make-message (constructor expression format args)
  (let (code)
    (when (plist-member args :code)
      (setq code (plist-get args :code))
      (setq args (map-delete args :code)))
    (funcall constructor
             :expression expression
             :message (apply 'format format args)
             :line (oref expression line)
             :column (oref expression column)
             :code code)))

(defun elsa--make-message-from-explainer (constructor expression explainer args)
  (let (compact)
    (when (plist-member args :compact)
      (setq compact (plist-get args :compact))
      (setq args (map-delete args :compact)))

    (when (and compact (= 2 (length (elsa-get-messages explainer))))
      (setf (elsa-get-messages explainer)
            (list (car (elsa-get-messages explainer)))))

    (elsa--make-message
     constructor
     expression
     (replace-regexp-in-string "%" "%%" (elsa-tostring explainer))
     args)))

(cl-defgeneric elsa-make-error (thing format &rest args)
  (declare (indent 1)))

(cl-defmethod elsa-make-error ((expression elsa-form) (format string) &rest args)
  (declare (indent 1))
  (elsa--make-message 'elsa-error expression format args))

(cl-defmethod elsa-make-error ((expression elsa-form) (explainer elsa-explainer) &rest args)
  "Create `elsa-error' object.

EXPRESSION is a form to which the error will be attached.

EXPLAINER is an instance of `elsa-explainer'.

If ARGS contains :compact t, if the explanation only has one
additional line, this line will be dropped, because most likely
it only reformulates the first line."
  (declare (indent 1))
  (elsa--make-message-from-explainer 'elsa-error expression explainer args))

(cl-defgeneric elsa-make-warning (thing format &rest args)
  (declare (indent 1)))

(cl-defmethod elsa-make-warning ((expression elsa-form) (format string) &rest args)
  (declare (indent 1))
  (elsa--make-message 'elsa-warning expression format args))

(cl-defmethod elsa-make-warning ((expression elsa-form)
                                 (explainer elsa-explainer)
                                 &rest args)
  "Create `elsa-warning' object.

EXPRESSION is a form to which the error will be attached.

EXPLAINER is an instance of `elsa-explainer'.

If ARGS contains :compact t, if the explanation only has one
additional line, this line will be dropped, because most likely
it only reformulates the first line."
  (declare (indent 1))
  (elsa--make-message-from-explainer 'elsa-warning expression explainer args))

(cl-defgeneric elsa-make-notice (thing format &rest args)
  (declare (indent 1)))

(cl-defmethod elsa-make-notice ((expression elsa-form)
                                (format string)
                                &rest args)
  (declare (indent 1))
  (elsa--make-message 'elsa-notice expression format args))

(cl-defmethod elsa-make-notice ((expression elsa-form)
                                (explainer elsa-explainer)
                                &rest args)
  "Create `elsa-notice' object.

EXPRESSION is a form to which the error will be attached.

EXPLAINER is an instance of `elsa-explainer'.

If ARGS contains :compact t, if the explanation only has one
additional line, this line will be dropped, because most likely
it only reformulates the first line."
  (declare (indent 1))
  (elsa--make-message-from-explainer 'elsa-notice expression explainer args))

(provide 'elsa-error)
;;; elsa-error.el ends here
