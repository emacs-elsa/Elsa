;;; elsa-types-simple.el --- Simple Elsa types -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created:  4th March 2023
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

(require 'elsa-type)
(require 'elsa-explainer)

(defclass elsa-type-unbound (elsa-type) ()
  :documentation "Type of an unbound variable.

This is not accepted by any type because we don't know what it is.")

(cl-defmethod elsa-type-accept ((_this elsa-type-unbound) _other &optional _explainer)
  "Unbound type accepts anything.

The only thing that can be of an unbound type is a symbol
representing a variable.  It can accept anything because it is
not bound to any specific value yet."
  t)

(cl-defmethod elsa-type-describe ((_this elsa-type-unbound))
  "unbound")

(defclass elsa-type-mixed (elsa-type) ()
  :documentation "Type of anything.

Mixed is a special type in a way that it serves as an opt-out
from type analysis.

Mixed accepts any other type except unbound.  This is
type-theoretically sound because mixed is the union of all types.

However, mixed is also accepted by any other type.  This is
unsafe and works as an implicit type-casting.

Typically, if some function returns mixed, such as reading a
property from a symbol with `get', we can wrap this call in a
function and provide a more narrow type signature for the return
type provided we know that the property is always of a certain
type.

  ;; (set-property :: (function (symbol int) int))
  (defun set-property (name value)
    (set name value))

  ;; (get-property :: (function (symbol) int))
  (defun get-property (name)
    ;; Even though `get' returns mixed, we know that if we only set it
    ;; with `set-property' it will always be int and so we can provide
    ;; more specific return type.
    (get name value))")

(cl-defmethod elsa-type-describe ((_this elsa-type-mixed))
  "mixed")

(cl-defmethod elsa-type-accept ((_this elsa-type-mixed) other &optional explainer)
  (if (not (cl-typep other 'elsa-type))
      (cl-call-next-method)
    (elsa-with-explainer explainer
      (elsa--fmt-explain-type-0-does-not-accept-type-1
       "mixed" (elsa-tostring other))
      (not (elsa-type-unbound-p other)))))

(provide 'elsa-types-simple)
;;; elsa-types-simple.el ends here
