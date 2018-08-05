;;; elsa.el --- Emacs Lisp Static Analyser -*- lexical-binding: t -*-

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

(require 'dash)

(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-state)
(require 'elsa-defun)
(require 'elsa-error)
(require 'elsa-analyser)
(require 'elsa-reader)

(require 'elsa-ruleset)

(require 'elsa-extension-builtin)

;; TODO: unify with `elsa-variable'?
(defclass elsa-defvar nil
  ((name :initarg :name)
   (type :initarg :type)))

(defclass elsa-expression nil
  ((type :initarg :type)))

(defun elsa-process-file (file)
  "Process FILE."
  (let ((buffer (find-file-noselect file))
        (state (elsa-state))
        (form))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (condition-case _err
              (while (setq form (elsa-read-form))
                (elsa-analyse-form state form))
            (end-of-file t)))))
    state))

(defun elsa-load-config ()
  "Load config and register extensions."
  (let ((config-buffer (find-file-noselect "Elsafile.el"))
        form)
    (with-current-buffer config-buffer
      (condition-case _err
          (while (setq form (read (current-buffer)))
            (pcase form
              (`(register-extensions . ,extensions)
               (--each extensions
                 (require (intern (concat "elsa-extension-" (symbol-name it))))))
              (`(register-ruleset . ,rulesets)
               (--each rulesets
                 (elsa-ruleset-load
                  (funcall (intern (concat "elsa-ruleset-" (symbol-name it)))))))))
        (end-of-file t)))))

(defun elsa-run ()
  "Run `elsa-process-file' and output errors to stdout for flycheck."
  (elsa-load-config)
  (let* ((file (car command-line-args-left))
         (state (elsa-process-file file))
         (errors (reverse (oref state errors))))
    (--each errors (princ (elsa-message-format it)))))

(defun elsa-analyse-form (state form &optional type)
  "Analyse FORM in STATE.

If TYPE is non-nil, force this type on FORM."
  (elsa--analyse-form form (oref state scope) state))

(provide 'elsa)
;;; elsa.el ends here
