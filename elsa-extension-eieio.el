;;; elsa-extension-eieio.el --- Elsa extension for EIEIO -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created:  6th March 2023
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

(require 'map)

(require 'dash)

(require 'elsa-explainer)
(require 'elsa-analyser)
(require 'elsa-type-helpers)
(require 'elsa-state)

(require 'elsa-typed-eieio)

(defun elsa--eieio-analyse-obj-slot (instance slot-form state)
  (when slot-form
    (let ((slot-name (elsa-get-name slot-form))
          (inst-type (elsa-get-type instance)))
      (when (elsa-class-type-p inst-type)
        (when-let* ((class (get (oref inst-type name) 'elsa-defclass)))
          (let* ((slots (elsa-get-slots class))
                 (slot-names (--map (oref it name) slots))
                 (slot (--find (eq slot-name (oref it name)) slots)))
            (oset slot-form type (or (and slot (oref slot type))
                                     (elsa-type-mixed)))
            (unless (memq slot-name slot-names)
              (elsa-state-add-message state
                (elsa-make-error slot-form
                  "Type `%s' has no slot `%s', has %s"
                  :code "eieio-invalid-slot"
                  (elsa-type-describe inst-type)
                  slot-name
                  (or slot-names "no slots"))))))))))

(defun elsa--eieio-assert-struct-for-obj (instance state)
  (when instance
    (let ((type (elsa-get-type instance)))
      ;; This test instead of `elsa-class-type-p' is so that mixed can
      ;; be used as a struct.
      (unless (elsa-type-assignable-p (elsa-make-type (class nil)) type)
        (elsa-state-add-message state
          (elsa-make-error instance
            "Type `%s' has no properties because it is not a class."
            :code "eieio-not-a-class"
            (elsa-type-describe type)))))))

(defun elsa--analyse:oref (form scope state)
  (let* ((instance (elsa-cadr form))
         (slot-form (elsa-nth 2 form)))
    (elsa--analyse-macro form (list t nil) scope state)
    (elsa--eieio-assert-struct-for-obj instance state)
    (elsa--eieio-analyse-obj-slot instance slot-form state)
    (oset form type (oref slot-form type))))

(defun elsa--analyse:oset (form scope state)
  (let* ((instance (elsa-cadr form))
         (slot-form (elsa-nth 2 form))
         (value-form (elsa-nth 3 form))
         (slot-name (elsa-get-name slot-form)))
    (elsa--analyse-macro form (list t nil t) scope state)
    (elsa--eieio-assert-struct-for-obj instance state)
    (elsa--eieio-analyse-obj-slot instance slot-form state)
    (unless (elsa-type-assignable-p
             (oref slot-form type)
             (oref value-form type))
      (elsa-state-add-message state
        (elsa-make-error slot-form
          (elsa-with-temp-explainer explainer
            (elsa-explain-and-indent explainer
              ("Property `%s' can not accept type `%s', has type `%s'"
               slot-name
               (elsa-tostring (oref value-form type))
               (elsa-tostring (oref slot-form type)))
              (elsa-type-accept (oref slot-form type) (oref value-form type) explainer))
            explainer)
          :code "eieio-invalid-type"
          :compact t)))))

(defun elsa-eieio--create-slots (slots)
  (let ((ht (make-hash-table)))
    (--each slots
      (let ((slot (elsa-structure-slot
                   :name (car it)
                   :type (plist-get (cdr it) :type)
                   :initarg (plist-get (cdr it) :initarg))))
        (puthash (oref slot name) slot ht)))
    ht))

(defun elsa--analyse:defclass (form _scope state)
  (let* ((name (elsa-get-name (elsa-cadr form)))
         (parents (elsa-nth 2 form))
         (parents-names (when (elsa-form-list-p parents)
                          (elsa-form-map parents #'elsa-get-name)))
         (slots (elsa-nth 3 form))
         (parents-tree (-fix
                        (lambda (parents)
                          (-mapcat
                           (lambda (p)
                             (if (symbolp p)
                                 (let* ((str (elsa-state-get-defclass state p))
                                        (pars (and str (oref str parents)))
                                        (pts (cdr (assq p pars))))
                                   (cons (cons p (copy-sequence pts))
                                         (copy-sequence pts)))
                               (list p)))
                           parents))
                        parents-names))
         (class nil))

    (setq
     class
     (elsa-defclass
      :name name
      :slots (elsa-eieio--create-slots
              (mapcar
               (lambda (slot)
                 (let* ((slot-name (elsa-get-name slot))
                        (slot-annotation (oref slot annotation))
                        (type-form (map-elt (elsa-cdr slot) :type))
                        (type-lisp (elsa-form-to-lisp type-form))
                        (elsa-type (or
                                    (and slot-annotation
                                         (elsa--make-type (nth 2 slot-annotation)))
                                    (and type-lisp
                                         (or (elsa--cl-type-to-elsa-type type-lisp)
                                             (elsa-type-mixed)))
                                    (elsa-type-mixed)))
                        (accessor (map-elt (elsa-cdr slot) :accessor))
                        (initarg (map-elt (elsa-cdr slot) :initarg)))

                   (when accessor
                     (elsa-state-add-method state
                       (elsa-defun
                        :name (elsa-get-name accessor)
                        :defun-type 'cl-defmethod
                        :type (elsa-function-type
                               :args (list (elsa--make-type `(class ,name)))
                               :return elsa-type)
                        :arglist (list 'this))))

                   (list slot-name
                         :type elsa-type
                         :initarg (elsa-get-name initarg))))
               (elsa-form-sequence slots)))
      :parents (let ((-compare-fn (-on #'eq #'car)))
                 (-uniq (cons (cons name parents-names) parents-tree)))))

    (elsa-state-add-defclass state class)

    ;; add the constructor
    (let ((kw-args (->> (elsa-get-slots class)
                        (--filter (oref it initarg))
                        (--map (list (oref it initarg)
                                     (oref it type)))
                        (elsa-type-make-plist-hashtable)
                        (elsa-type-keys :slots))))
      (elsa-state-add-defun state
        (elsa-defun
         :name name
         :type (elsa-function-type
                :args (list kw-args)
                :return (elsa--make-type `(class ,name)))
         :arglist (list '&rest 'args))))

    ;; add the type predicate
    (elsa-state-add-defun state
      (elsa-defun
       :name (intern (concat (symbol-name name) "-p"))
       :type (elsa--make-type `(function (mixed) (is (class ,name))))
       :arglist (list 'x)))))

(provide 'elsa-extension-eieio)
;;; elsa-extension-eieio.el ends here
