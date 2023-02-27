;;; elsa-declare.el --- Elsa cache declaration macros -*- lexical-binding: t -*-

(require 'elsa-state)
(require 'elsa-extension-eieio)
(require 'elsa-type-helpers)

(defmacro elsa-in-file (file)
  "Specify the current file for registering new structures."
  `(oset elsa-global-state current-file ,file))

(defmacro elsa-declare-defun (name arglist type)
  (declare (indent 2))
  `(elsa-state-add-defun elsa-global-state
     (elsa-defun :name ',name :type (elsa-make-type ,type) :arglist ',arglist)))

(defmacro elsa-declare-defvar (name type)
  (declare (indent 1))
  `(elsa-state-add-defvar elsa-global-state
     (elsa-defvar :name ',name :type (elsa-make-type ,type))))

(defmacro elsa-declare-structure (name parents slots)
  (declare (indent 2))
  `(elsa-state-add-structure elsa-global-state
     (elsa-cl-structure :name ',name
                        :parents ',(or parents `((,name)))
                        :slots (elsa-eieio--create-slots
                                (list ,@(mapcar
                                         (lambda (slot)
                                           `(list ',(car slot)
                                                  :type (elsa--make-type ',(plist-get (cdr slot) :type))))
                                         slots))))))

(provide 'elsa-declare)
;;; elsa-declare.el ends here
