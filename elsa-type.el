;;; elsa-type.el --- Elsa type -*- lexical-binding: t -*-

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

(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))

(defconst elsa--fmt-explain-type-0-does-not-accept-type-1
  "Type `%s' does not accept type `%s'")

(defconst elsa--fmt-explain-types-of-slot-0-do-not-match
  "Types of slot `%s' do not match")

(defconst elsa--fmt-explain-slot-0-not-found-on-1
  "Slot `%s' not found on `%s'")

(defconst elsa--fmt-explain-0-is-not-a-1
  "`%s' is not a `%s'")

(defconst elsa--fmt-explain-type-0-only-accepts-type-1-was-2
  "Type `%s' only accepts type `%s', was `%s'")

(defconst elsa--fmt-explain-sequence-item-types-of-these-0-do-not-match
  "Sequence item types of these %s do not match.")

(defclass elsa-type nil () :abstract t)

;; (elsa-type-sum :: (function (mixed mixed) (class elsa-type)))
(cl-defgeneric elsa-type-sum (this other)
  "Return the sum of THIS and OTHER type.

A sum accept anything that either THIS or OTHER accepts.")

;; (elsa-type-dif :: (function (mixed mixed) (class elsa-type)))
(cl-defgeneric elsa-type-diff (this other)
  "Return the difference of THIS without OTHER.

The diff type only accepts those types accepted by THIS which are
not accepted by OTHER.")

;; (elsa-type-intersect :: (function (mixed mixed) (class elsa-type)))
(cl-defgeneric elsa-type-intersect (this other)
  "Return the intersection of THIS and OTHER.

The intersection type only accepts those types which are both
THIS and OTHER at the same time.")

;; (elsa-type-normalize :: (function ((class elsa-type)) (class elsa-type)))
(cl-defgeneric elsa-type-normalize (type)
  "Normalize TYPE to its most simplest form.")

;; (elsa-type-describe :: (function (mixed) string))
(cl-defgeneric elsa-type-describe (type)
  "Return a string representation of TYPE."
  (format "%s" type))

;; FIXME: rename to elsa-make-non-nullable
;; (elsa-type-make-non-nullable :: (function (mixed) mixed))
(cl-defgeneric elsa-type-make-non-nullable (type)
  "Make TYPE non-nullable.

This is the same as diffing nil from type.")

(provide 'elsa-type)
;;; elsa-type.el ends here
