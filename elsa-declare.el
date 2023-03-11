;;; elsa-declare.el --- Elsa cache declaration macros -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created:  5th March 2023

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

;; The macros here are used only in the elsa cache files (stored by
;; default in .elsa in the current directory).  They should not be
;; used to declare types in your code, for that, see elsa-macros.el.

;;; Code:

(require 'elsa-state)
(require 'elsa-extension-eieio)
(require 'elsa-type-helpers)

(defmacro elsa-in-file (file)
  "Specify the current file for registering new structures."
  `(oset elsa-global-state current-file ,file))

(defmacro elsa-declare-defun (name arglist type)
  (declare (indent 2))
  `(elsa-state-add-method elsa-global-state
     (elsa-defun :name ',name :type (elsa-make-type ,type) :arglist ',arglist)))

(defmacro elsa-declare-defgeneric (name arglist type)
  (declare (indent 2))
  `(elsa-state-add-defun elsa-global-state
     (elsa-defun :name ',name
                 :type (elsa-make-type ,type)
                 :defun-type 'cl-defgeneric
                 :defgeneric-type (elsa-make-type ,type)
                 :arglist ',arglist)))

(defmacro elsa-declare-defvar (name type)
  (declare (indent 1))
  `(elsa-state-add-defvar elsa-global-state
     (elsa-defvar :name ',name :type (elsa-make-type ,type))))

(defmacro elsa-declare-defstruct (name parents slots)
  (declare (indent 2))
  `(elsa-state-add-defstruct elsa-global-state
     (elsa-defstruct :name ',name
                     :parents ',(or parents `((,name)))
                     :slots (elsa-eieio--create-slots
                             (list ,@(mapcar
                                      (lambda (slot)
                                        `(list ',(car slot)
                                               :type (elsa--make-type ',(plist-get (cdr slot) :type))))
                                      slots))))))

(defmacro elsa-declare-defclass (name parents slots)
  (declare (indent 2))
  `(elsa-state-add-defclass elsa-global-state
     (elsa-defclass :name ',name
                     :parents ',(or parents `((,name)))
                     :slots (elsa-eieio--create-slots
                             (list ,@(mapcar
                                      (lambda (slot)
                                        `(list ',(car slot)
                                               :type (elsa--make-type ',(plist-get (cdr slot) :type))))
                                      slots))))))

(provide 'elsa-declare)
;;; elsa-declare.el ends here
