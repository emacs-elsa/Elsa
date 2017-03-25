;;; elsa-types.el --- Elsa types -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 23rd March 2017
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

(defun elsa-type--get-class-constructor (type)
  "Return constructor information for TYPE.

If the TYPE starts with elsa-type prefix, it is returned as-is.
Otherwise, elsa-type- is prefixed.

If the TYPE is suffixed with `?', the type is recognized as
nullable.

The return value is a plist with :constructor being the
constructor and :nullable a boolean specifying if the type is
nullable.

Constructor is a symbol which is the actual EIEIO type
representing TYPE."
  (let* ((name (split-string (symbol-name type) "?"))
         (class (intern
                 (if (string-prefix-p "elsa-type" (car name))
                     (car name)
                   (concat "elsa-type-"  (car name))))))
    (list :constructor class :nullable (equal (cadr name) ""))))

(defun elsa-make-type (definition)
  "Return instance of class representing DEFINITION."
  (-let (((&plist :constructor c :nullable n)
          (elsa-type--get-class-constructor definition)))
    (make-instance c :nullable n)))

(defun elsa--eieio-class-parents-recursive (type)
  "Return all parents of TYPE."
  (cons type
        (-mapcat 'elsa--eieio-class-parents-recursive (eieio-class-parents type))))

;; TODO: what is the relationship of `a' and `a?'
(defun elsa-instance-of (this other)
  "Non-nil if THIS is instance of OTHER."
  (let ((this-type
         (if (symbolp this)
             (plist-get (elsa-type--get-class-constructor this) :constructor)
           (eieio-object-class this)))
        (other-type
         (if (symbolp other)
             (plist-get (elsa-type--get-class-constructor other) :constructor)
           (eieio-object-class other))))
    (not (null
          (memq other-type (elsa--eieio-class-parents-recursive this-type))))))

(defclass elsa-type nil
  ((nullable :type boolean
             :initarg :nullable
             :initform nil))
  :abstract t)

(defmethod elsa-type-describe ((this elsa-type))
  (error "Not implemented yet"))

(defmethod elsa-type-accept ((this elsa-type) other)
  (error "Not implemented yet"))

(defmethod elsa-type-combine-with ((this elsa-type) other)
  (error "Not implemented yet"))

(defmethod elsa-type-nullable-p ((this elsa-type))
  (oref this nullable))

(defmethod elsa-type-make-nullable ((this elsa-type))
  (make-instance (eieio-object-class this) :nullable t))

(defclass elsa-type-trait-just-nullable nil
  ()
  :abstract t)

(defmethod elsa-type-accept ((this elsa-type-trait-just-nullable) other)
  (cond
   ((and (elsa-instance-of other this)
         (or (elsa-type-nullable-p this)
             (not (elsa-type-nullable-p other)))))
   ((and (elsa-type-nullable-p this)
         (elsa-type-nil-p other)))
   ((and (elsa-sum-type-p other)
         (-all? (lambda (other-type)
                  (elsa-type-accept this other-type))
                (oref other types))))
   (t nil)))

(defmethod elsa-type-combine-with ((this elsa-type-trait-just-nullable) other)
  (cond
   ((elsa-type-nil-p other)
    (elsa-type-make-nullable this))
   ((elsa-type))
   ;; (t (elsa-type-mixed ""))
   ;; ((elsa-sum-type-p other)
   ;;  (elsa-sum-type-add other this))
   ;; ((elsa-type-p other)
   ;;  (elsa-sum-type "" :types (list this other)))
   ))

(defclass elsa-sum-type (elsa-type)
  ((types :type list
          :initarg :types
          :initform nil)))

(defmethod elsa-sum-type-add ((this elsa-sum-type) other)
  (unless (elsa-type-child-p other) (error "Other must be `elsa-type'"))
  (if (elsa-type-nil-p other)
      (oset this nullable t)
    (oset this types (cons other (oref this types)))))

(defmethod elsa-type-accept ((this elsa-sum-type) other)
  (-any? (lambda (ot) (elsa-type-accept ot other)) (oref this types)))

(defmethod elsa-type-nullable-p ((this elsa-sum-type))
  (or (oref this nullable)
      (-any? 'elsa-type-nullable-p (oref this types))))

(defclass elsa-type-nil (elsa-type) ())

(defmethod elsa-type-accept ((this elsa-type-nil) other)
  (elsa-type-nil-p other))

(defmethod elsa-type-describe ((this elsa-type-nil))
  "nil")

(defclass elsa-type-mixed (elsa-type) ())

(defmethod elsa-type-describe ((this elsa-type-mixed))
  "mixed")

(defmethod elsa-type-accept ((this elsa-type-mixed) _other)
  t)

(defmethod elsa-type-nullable-p ((this elsa-type-mixed))
  t)

(defclass elsa-type-string (elsa-type-trait-just-nullable elsa-type) ())

(defclass elsa-type-short-string (elsa-type-string) ())

(defmethod elsa-type-describe ((this elsa-type-string))
  "string")

(defclass elsa-type-buffer (elsa-type) ())

(defmethod elsa-type-describe ((this elsa-type-buffer))
  "buffer")

(provide 'elsa-types)
;;; elsa-types.el ends here
