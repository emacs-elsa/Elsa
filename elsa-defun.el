;;; elsa-defun.el --- elsa-defun -*- lexical-binding: t -*-

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

(require 'dash)

(require 'elsa-types)

(defclass elsa-defun nil
  ((name :initarg :name)
   (args :initarg :args)
   (return-type :initarg :return-type)))

(defun elsa-defun--get-return-type (declarations)
  "Get return type from DECLARATIONS."
  (-when-let (return-type (cadr (--first (eq (car it) 'elsa-return) declarations)))
    (elsa-make-type return-type)))

(defun elsa-defun--get-typed-args (args types)
  "Attach types to ARGS according to TYPES."
  (let ((re nil))
    (-each args
      (lambda (arg)
        (if (memq arg '(&optional &rest &key))
            (push (list arg) re)
          (push (cons arg
                      (elsa-make-type
                       (if (consp types) (car types) 'mixed)))
                re)
          (!cdr types))))
    (nreverse re)))

(defun elsa-make-defun (name args declarations)
  "Make a new defun."
  (elsa-defun
   ""
   :name name
   :args (elsa-defun--get-typed-args
          args (cdr (--first (eq (car it) 'elsa-args) declarations)))
   :return-type (elsa-defun--get-return-type declarations)))

(provide 'elsa-defun)
;;; elsa-defun.el ends here
