;;; elsa.el --- Emacs Lisp Static Analyser -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 23rd March 2017
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (trinary "1.0.0") (f "0") (dash "2.14") (cl-lib "0.3"))
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

(require 'jka-compr)
(require 'eieio)

(require 'dash)
(require 'cl-lib)
(require 'cl-extra)
(require 'warnings)

(require 'elsa-dependencies)
(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-state)
(require 'elsa-error)
(require 'elsa-analyser)
(require 'elsa-reader)
(require 'elsa-log)

(require 'elsa-ruleset)

(require 'elsa-extension-builtin)

(require 'elsa-typed-syntax)
 ;; TODO: figure out how to load these based on `(require)' forms
(require 'elsa-typed-thingatpt)
(require 'elsa-typed-subr)

;; (toggle-debug-on-error)

;; TODO: unify with `elsa-variable'?
(defclass elsa-defvar nil
  ((name :initarg :name)
   (type :initarg :type)))

(defclass elsa-global-state nil
  ((project-directory :type string
                      :initarg :project-directory
                      :documentation "Directory from which the analysis was started.")
   (processed-file-index :type integer
                         :initarg :processed-file-index
                         :initform 1
                         :documentation "Index of currently processed file.")
   (number-of-files :type integer
                    :initarg :number-of-files
                    :documentation "Total number of files to analyze.")
   (defuns :type hash-table
     :initarg :defuns
     :initform (make-hash-table)
     :documentation "Defun definitions loaded from cache."))
  :documentation "Initial configuration and state of the entire analysis.

The other state class `elsa-state' state holds state of an analysis of
a single file, form or set of forms.")

(cl-defmethod elsa-global-state-get-counter ((this elsa-global-state))
  (let ((max-len (length (number-to-string (oref this number-of-files)))))
    (format (format "%%%dd/%%%dd" max-len max-len)
            (oref this processed-file-index)
            (oref this number-of-files))))

(cl-defmethod elsa-global-state-prefix-length ((this elsa-global-state) &optional extra)
  (let ((max-len (length (number-to-string (oref this number-of-files)))))
    (+ max-len 1 max-len (or extra 0))))

(cl-defmethod elsa-state-add-defun ((this elsa-global-state) (def elsa-defun))
  (put (oref def name) 'elsa-type (oref def type))
  (puthash (oref def name) def (oref this defuns)))

(defvar global-state (elsa-global-state))

(defmacro elsa-declare-defun (name arglist type)
  (declare (indent 2))
  `(elsa-state-add-defun global-state
     (elsa-defun :name ',name :type (elsa-make-type ,type) :arglist ',arglist)))

(defun elsa--get-cache-file-name (global-state feature &optional compiled)
  "Return the cache file name for LIBRARY."
  (f-expand (format ".elsa/%s-elsa-cache.el%s"
                    (if (stringp feature) feature (symbol-name feature))
                    (if compiled "c" ""))
            (oref global-state project-directory)))

(defun elsa-analyse-file (file)
  (let ((global-state (elsa-global-state :project-directory (f-parent file)))
        (dependencies (cons
                       "subr" ; subr is always a dependency
                       (elsa-get-dependencies file)))
        (visited nil)
        (file-state nil))
    (elsa-log "Processing transitive dependencies: %s" (s-join ", " dependencies))
    (oset global-state number-of-files (length dependencies))
    (dolist (dep dependencies)
      (let* ((dep (replace-regexp-in-string "-CIRCULAR\\'" "" dep))
             (elsa-cache-file (elsa--get-cache-file-name global-state dep)))
        (when-let* ((library (if (f-exists? dep) dep
                               (elsa--find-dependency dep))))
          (if (member library visited)
              (elsa-log "[%s] Skipping already processed dependency %s (was circular)"
                        (elsa-global-state-get-counter global-state) dep)
            (push library visited)
            (when (require (intern (concat "elsa-typed-"
                                           (replace-regexp-in-string ".el\\'" "" dep)))
                           nil t)
              (elsa-log "%sAutoloading types for %s"
                        (make-string (elsa-global-state-prefix-length global-state 3) ? )
                        dep))
            (when (require (intern (concat "elsa-extension-"
                                           (replace-regexp-in-string ".el\\'" "" dep)))
                           nil t)
              (elsa-log "%sAutoloading extension for %s"
                        (make-string (elsa-global-state-prefix-length global-state 3) ? )
                        dep))
            (if (file-newer-than-file-p library elsa-cache-file)
                (let ((state (elsa-process-file library global-state)))
                  ;; `subr' has no provide for some circular
                  ;; dependency "bootstrap" issues.  We add it here
                  ;; artificially.
                  (when (equal dep "subr")
                    (oset state provide (list 'subr)))
                  (elsa-save-cache state global-state)
                  ;; copy new definitions to the global state
                  (maphash
                   (lambda (_name def)
                     (elsa-state-add-defun global-state def))
                   (oref state defuns))
                  (setq file-state state))
              (elsa-log "[%s] Loading %s from cache %s"
                        (elsa-global-state-get-counter global-state) dep elsa-cache-file)
              (load (f-no-ext elsa-cache-file) t t)))))
      (cl-incf (oref global-state processed-file-index)))
    file-state))

(defun elsa--setup-buffer (state)
  "Prepare the current buffer for analysis.

STATE is the currently used state holding Elsa's knowledge.

This function annotates lines with line numbers so they can be
retrieved from text properties (fast!) instead of manually
recomputed each time by counting newlines.

It also resolves the elsa-disable-line and elsa-disable-next-line
tokens."
  (save-excursion
    (goto-char (point-min))
    (let ((line 1)
          (has-more t))
      (while has-more
        (put-text-property (point) (min (buffer-size) (1+ (point)))
                           'elsa-line line)
        (save-excursion
          (end-of-line)
          ;; 12 is comment ender, newline in elisp
          (when (and (or (eobp)
                         (eq (syntax-class (syntax-after (point))) 12))
                     (nth 4 (syntax-ppss)))
            (cond
             ((search-backward "elsa-disable-line" (line-beginning-position) t)
              (elsa-state-ignore-line state line))
             ((search-backward "elsa-disable-next-line" (line-beginning-position) t)
              (elsa-state-ignore-line state (1+ line))))))
        (setq has-more (= (forward-line) 0))
        (cl-incf line)))))

;; (elsa-process-file :: (function (string mixed) mixed))
(defun elsa-process-file (file global-state)
  "Process FILE."
  (elsa-log "[%s] Processing file %s"
            (elsa-global-state-get-counter global-state) file)
  (let ((state (elsa-state))
        (form))
    (oset state global-defuns (oref global-state defuns))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (elsa--setup-buffer state)
      (goto-char (point-min))
      (while (setq form (elsa-read-form state))
        (elsa-analyse-form state form)))
    state))

(defun elsa-save-cache (state global-state)
  "Save all cached defuns from STATE.

GLOBAL-STATE is the initial configuration."
  (with-temp-buffer
    (when-let ((features (oref state provide)))
      (dolist (feature features)
        (let ((feature-name (symbol-name feature))
              (elsa-cache-file (elsa--get-cache-file-name global-state feature)))
          (unless (string-match-p "^elsa-\\(typed\\|extension\\)-" feature-name)
            (progn
              (maphash
               (lambda (name def)
                 (insert
                  (format
                   "%S\n"
                   `(elsa-declare-defun ,name ,(when (slot-boundp def 'arglist) (oref def arglist))
                      ,(read (elsa-type-describe (oref def type))))
                   ;; `(elsa-state-add-defun global-state
                   ;;    (elsa-defun :name ',name
                   ;;                :type (elsa-make-type ,(read (elsa-type-describe (oref def type))))
                   ;;                ,@(when (slot-boundp def 'arglist)
                   ;;                    (list :arglist `',(oref def arglist)))))
                   )))
               (oref state defuns))
              (-each (nreverse (oref state requires))
                (lambda (req)
                  (insert (format ";; %S\n" `(elsa-load-cache ',req)))))
              (f-mkdir (f-parent elsa-cache-file))
              (f-write-text (buffer-string) 'utf-8 elsa-cache-file)
              (byte-compile-file elsa-cache-file))))))))

(defun elsa-process-form ()
  "Read and analyse form at point."
  (interactive)
  (let* ((state (elsa-state :project-directory (f-parent (buffer-file-name)))))
    (when-let ((form (elsa-read-form state)))
      (elsa-analyse-form state form)
      (list :form form :state state))))

(defun elsa-load-config ()
  "Load config and register extensions."
  (elsa-ruleset-load (elsa-ruleset-default))
  ;; enable auto-compression-mode for reading of sources
  (auto-compression-mode 1)
  ;; silence the "uncompressing" messages
  (setq jka-compr-verbose nil)
  ;; make the available stack bigger
  (setq max-specpdl-size 1000)
  (setq max-lisp-eval-depth 1000)

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
  "Run `elsa-analyse-file' and output errors to stdout for flycheck."
  (elsa-load-config)
  (dolist (file command-line-args-left)
    (--each (reverse (oref (elsa-analyse-file file) errors))
      (princ (concat file ":" (elsa-message-format it))))))

(defun elsa-run-files-and-exit ()
  "Run `elsa-analyse-file' on files in `command-line-args-left'.
Output errors to stdout, and exit Emacs when done, non-zero if
errors are found."
  (elsa-load-config)
  (let (exit-code)
    (dolist (file command-line-args-left)
      (when-let* ((errors (oref (elsa-analyse-file file) errors)))
        (setf exit-code 1)
        (--each (reverse errors)
          (princ (concat file ":" (elsa-message-format it))))))
    (kill-emacs exit-code)))

(defun elsa-analyse-form (state form &optional _type)
  "Analyse FORM in STATE.

If TYPE is non-nil, force this type on FORM."
  (let ((scope (oref state scope)))
    (elsa--analyse-form form scope state)))

(provide 'elsa)
;;; elsa.el ends here
