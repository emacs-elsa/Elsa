;;; elsa-variable.el --- elsa-variable -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 25th March 2017
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

;; TODO: maybe move the acceptance resolution into an expression type?
(defclass elsa-variable nil
  ((name :initarg :name)
   (type :initarg :type
         :documentation "Type of this variable for assignment.

You can assign to this variable any expression which is accepted
by this type.")
   (surely :initform nil
           :documentation "This variable is surely of this type.")
   (surely-not :initform nil
               :documentation "This variable is surely not of this type."))
  :documentation "A lexical variable")

(defmethod elsa-variable-surely ((this elsa-variable) type)
  (unless (elsa-type-child-p type) (error "Type must be `elsa-type-child-p'"))
  (oset this surely (cons type (oref this surely))))

(defmethod elsa-variable-pop-surely ((this elsa-variable))
  (oset this surely (cdr (oref this surely))))

(defmethod elsa-variable-surely-not ((this elsa-variable) type)
  (unless (elsa-type-child-p type) (error "Type must be `elsa-type-child-p'"))
  (oset this surely-not (cons type (oref this surely-not))))

(defmethod elsa-variable-pop-surely-not ((this elsa-variable))
  (oset this surely-not (pop (oref this surely-not))))

(defmethod elsa-variable-is-accepted ((this elsa-variable) type)
  "Return non-nil if VARIABLE is accepted by TYPE."
  (unless (elsa-type-child-p type) (error "Type must be `elsa-type-child-p'"))
  (and (or (not (oref this surely))
           (elsa-type-accept type (oref this surely)))
       (or (not (oref this surely-not))
           (not (elsa-type-accept type (oref this surely-not))))
       (elsa-type-accept type (oref this type))))

(provide 'elsa-variable)
;;; elsa-variable.el ends here
