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
(require 'cl-extra)
(require 'warnings)

(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-state)
(require 'elsa-error)
(require 'elsa-analyser)
(require 'elsa-reader)

(require 'elsa-ruleset)

(require 'elsa-extension-builtin)

(require 'elsa-typed-syntax)
 ;; TODO: figure out how to load these based on `(require)' forms
(require 'elsa-typed-thingatpt)
(require 'elsa-typed-subr)

;; TODO: unify with `elsa-variable'?
(defclass elsa-defvar nil
  ((name :initarg :name)
   (type :initarg :type)))

(defun elsa-process-file (file)
  "Process FILE."
  (let ((state (elsa-state))
        (form))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (let ((line 1))
        (put-text-property (point) (1+ (point)) 'elsa-line line)
        (while (= (forward-line) 0)
          (cl-incf line)
          (put-text-property (point) (min
                                      (buffer-size)
                                      (1+ (point))) 'elsa-line line)
          (save-excursion
            (end-of-line)
            ;; 12 is comment ender, newline in elisp
            (when (and (eq (syntax-class (syntax-after (point))) 12)
                       (search-backward "elsa-disable-line"
                                        (line-beginning-position)
                                        t)
                       (nth 4 (syntax-ppss)))
              (elsa-state-ignore-line state line)))))
      (goto-char (point-min))
      (condition-case _err
          (while (setq form (elsa-read-form state))
            (elsa-analyse-form state form))
        (end-of-file t)))
    ;; dump defun cache
    ;; FIXME: temporarily disable caching since new-style structs can't be read.
    ;; (with-temp-buffer
    ;;   (let ((elsa-cache-file (elsa--get-cache-file-name file)))
    ;;     (unwind-protect
    ;;         (progn
    ;;           (-each (nreverse (oref state defuns))
    ;;             (lambda (dfn)
    ;;               (insert (format "%s\n" `(put (quote ,(cadr dfn)) 'elsa-type ,(nth 2 dfn))))))
    ;;           (f-mkdir (f-parent elsa-cache-file))
    ;;           (f-write-text
    ;;            (buffer-string) 'utf-8
    ;;            elsa-cache-file)
    ;;           (byte-compile-file elsa-cache-file))
    ;;       (f-delete elsa-cache-file))))
    state))

(defun elsa-process-form ()
  "Read and analyse form at point."
  (interactive)
  (let* ((state (elsa-state))
         (form (elsa-read-form state)))
    (elsa--analyse-form form (oref state scope) state)
    form))

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
                 (unless (require (intern (concat "elsa-extension-" (symbol-name it))) nil t)
                   (princ (format "An error occured during startup: Extension %s not found\n" (symbol-name it))))))
              (`(register-ruleset . ,rulesets)
               (--each rulesets
                 (let ((ruleset-constructor (intern (concat "elsa-ruleset-" (symbol-name it)))))
                   (if (functionp ruleset-constructor)
                       (elsa-ruleset-load (funcall ruleset-constructor))
                     (princ (format "An error occured during startup: Ruleset %s not found\n" (symbol-name it)))))))))
        (end-of-file t)))))

(defun elsa-run ()
  "Run `elsa-process-file' and output errors to stdout for flycheck."
  (elsa-load-config)
  (dolist (file command-line-args-left)
    (--each (reverse (oref (elsa-process-file file) errors))
      (princ (concat file ":" (elsa-message-format it))))))

(defun elsa-run-files-and-exit ()
  "Run `elsa-process-file' on files in `command-line-args-left'.
Output errors to stdout, and exit Emacs when done, non-zero if
errors are found."
  (elsa-load-config)
  (let (exit-code)
    (dolist (file command-line-args-left)
      (when-let* ((errors (oref (elsa-process-file file) errors)))
        (setf exit-code 1)
        (--each (reverse errors)
          (princ (concat file ":" (elsa-message-format it))))))
    (kill-emacs exit-code)))

(defun elsa-analyse-form (state form &optional _type)
  "Analyse FORM in STATE.

If TYPE is non-nil, force this type on FORM."
  (elsa--analyse-form form (oref state scope) state))

(provide 'elsa)
;;; elsa.el ends here
