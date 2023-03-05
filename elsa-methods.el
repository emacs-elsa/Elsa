;;; elsa-methods.el --- Common Elsa generic methods -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 14th February 2023
;; Package-requires: ((dash "2.17.0"))
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

;; (elsa-get-name :: (function (mixed) (or symbol nil)))
(cl-defgeneric elsa-get-name (_this)
  "Retrieve name from an Elsa object."
  nil)

;; (elsa-get-type :: (function (mixed) (or (class elsa-type) nil)))
(cl-defgeneric elsa-get-type (_this)
  "Return type of THIS."
  nil)

;; (elsa-tostring :: (function (mixed) string))
(cl-defgeneric elsa-tostring (this)
  "Convert THIS to string."
  (format "%S" this))

(provide 'elsa-methods)
;;; elsa-methods.el ends here
