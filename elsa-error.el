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

(defclass elsa-message ()
  ((message :initarg :message)
   (line :initarg :line :initform nil)
   (column :initarg :column
           :initform nil)
   (expression :initarg :expression))
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

(cl-defmethod elsa-message-type ((_this elsa-message))
  "message")

(cl-defmethod elsa-message-type ((_this elsa-error))
  "error")

(cl-defmethod elsa-message-type ((_this elsa-warning))
  "warning")

(cl-defmethod elsa-message-type ((_this elsa-notice))
  "notice")

(cl-defmethod elsa-message-format ((this elsa-message))
  "Format an `elsa-message'."
  (format "%s:%s:%s:%s\n"
          (oref this line)
          (or (oref this column) "?")
          (elsa-message-type this)
          (oref this message)))

(defun elsa--make-message (constructor expression format args)
  (funcall constructor
           :expression expression
           :message (apply 'format format args)
           :line (oref expression line)
           :column (oref expression column)))

(defun elsa-make-error (expression format &rest args)
  (declare (indent 1))
  (elsa--make-message 'elsa-error expression format args))

(defun elsa-make-warning (expression format &rest args)
  (declare (indent 1))
  (elsa--make-message 'elsa-warning expression format args))

(defun elsa-make-notice (expression format &rest args)
  (declare (indent 1))
  (elsa--make-message 'elsa-notice expression format args))

(provide 'elsa-error)
;;; elsa-error.el ends here
