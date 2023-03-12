;;; elsa-structure-slot.el --- Elsa structure slot -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 10th March 2023
;; Package-requires: ((dash "2.17.0"))
;; Keywords:

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

(require 'elsa-methods)

(defclass elsa-structure-slot nil
  ((name
    :type symbol
    :initarg :name
    :accessor elsa-get-name
    :documentation "Slot name.")
   (initarg
    :type (or symbol null)
    :initarg :initarg
    :initform nil
    :documentation "Symbol or keyword used to initialize the slot.")
   (type
    :type elsa-type
    :initarg :type
    :accessor elsa-get-type
    :documentation "Slot Elsa type."))
  :documentation "Data about a slot in interface-like structure.

An interface-like structure is anything that has defined set of keys
and associated values.  In elisp, these can be the ad-hoc data
structures such as plists and alists and more structured cases like
`cl-defstruct' and `defclass'.")

(cl-defmethod initialize-instance ((this elsa-structure-slot) &optional slots)
  (cl-call-next-method)
  (let ((initarg (plist-get slots :initarg))
        (name (plist-get slots :name)))
    (unless initarg
      (oset this initarg name))))

(cl-defmethod cl-print-object ((this elsa-structure-slot) stream)
  (princ
   (format "#<elsa-structure-slot %s %s %s>"
           (oref this name)
           (oref this initarg)
           (elsa-tostring (oref this type)))
   stream))

(provide 'elsa-structure-slot)
;;; elsa-structure-slot.el ends here
