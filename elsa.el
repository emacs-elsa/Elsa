;;; elsa.el --- Emacs Lisp Static Analyser -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 23rd March 2017
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (trinary "0") (f "0") (dash "2.14") (cl-lib "0.3") (lsp-mode "0") (ansi "0") (async "1.9.7") (lgr "0.1.0"))
;; URL: https://github.com/emacs-elsa/Elsa
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

(defvar elsa-is-language-server nil)

(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))
(require 'cl-lib)
(require 'cl-extra)
(require 'warnings)
(require 'jka-compr)

(require 'dash)
(require 'ansi)
(require 'async)

(require 'elsa-dependencies)
(require 'elsa-types)
(require 'elsa-scope)
(require 'elsa-state)
(require 'elsa-error)
(require 'elsa-analyser)
(require 'elsa-reader)
(require 'elsa-log)
(require 'elsa-declare)

(require 'elsa-ruleset)

(require 'elsa-extension-builtin)

(require 'elsa-typed-syntax)
 ;; TODO: figure out how to load these based on `(require)' forms
(require 'elsa-typed-thingatpt)
(require 'elsa-typed-subr)

;; (toggle-debug-on-error)

;; TODO: unify with `elsa-variable'?

(defun elsa--get-cache-file-name (global-state feature &optional compiled)
  "Return the cache file name for LIBRARY."
  (f-expand (format ".elsa/%s-elsa-cache.el%s"
                    (if (stringp feature) feature (symbol-name feature))
                    (if compiled "c" ""))
            (oref global-state project-directory)))

(defun elsa-analyse-form (state form &optional _type)
  "Analyse FORM in STATE.

If TYPE is non-nil, force this type on FORM."
  (let ((scope (oref state scope)))
    (elsa--analyse-form form scope state)))

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

;; (elsa-process-file :: (function (string (or (class elsa-global-state) nil)) mixed))
(defun elsa-process-file (file &optional global-state no-log)
  "Process FILE."
  (setq global-state (or global-state elsa-global-state))
  (unless no-log
    (elsa-log
     (with-ansi
      (green "[%s]" (elsa-global-state-get-counter global-state))
      (format " Processing file %s ..." file))))
  (let ((lgr (lgr-get-logger "elsa.process.file"))
        (state (elsa-state :global-state global-state))
        (current-time (current-time))
        (form))
    (elsa-state-clear-file-state global-state file)
    (oset global-state current-file file)
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (elsa--setup-buffer state)
      (goto-char (point-min))
      (if elsa-is-language-server
          ;; TODO: during the initial load/analyse of all
          ;; dependencies, we should somehow track which files
          ;; produced errors, because this can impact the analysis
          ;; correctness.
          (while (setq form
                       (condition-case err
                           (elsa-read-form state)
                         (error
                          (elsa-state-add-message state
                            (let ((err-form
                                   (save-excursion
                                     (beginning-of-defun)
                                     (forward-symbol 1)
                                     (forward-symbol -1)
                                     (elsa--read-symbol (symbol-at-point)))))
                              (elsa--set-line-and-column err-form)
                              (elsa-make-error err-form
                                "Elsa could not read form %s: %s"
                                (elsa-tostring err-form)
                                (error-message-string err))))
                          (end-of-defun)
                          :skip)))
            (condition-case err
                (unless (eq form :skip)
                  (elsa-analyse-form state form))
              (error (elsa-state-add-message state
                       (elsa-make-error form
                         "Elsa could not analyse form %s: %s"
                         (elsa-tostring form)
                         (error-message-string err))))))
        ;; When not running as language server, just crash on errors.
        (condition-case nil
            (while (setq form (elsa-read-form state))
              (lgr-trace lgr
                "Processing form %s"
                (replace-regexp-in-string "%" "%%%%" (elsa-tostring form)))
              (elsa-analyse-form state form))
          (end-of-file t))))
    (unless no-log
      (elsa-log
       (with-ansi
        (previous-line)
        (kill)
        (green "[%s]" (elsa-global-state-get-counter global-state))
        (format " Processing file %s ... " file)
        (green "done")
        " after "
        (let ((duration (float-time
                         (time-subtract
                          (current-time) current-time))))
          (cond
           ((> duration 5)
            (bright-red "%.3fs" duration))
           ((> duration 2)
            (bright-yellow "%.3fs" duration))
           (t (green "%.3fs" duration)))))))
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
              (insert (format "%S\n" `(elsa-in-file ,(oref global-state current-file))))
              (maphash
               (lambda (name def)
                 (insert
                  (format
                   "%S\n"
                   `(elsa-declare-defstruct ,name ,(oref def parents)
                      ,(mapcar (lambda (slot)
                                 (list (oref slot name)
                                       :type (read (elsa-type-describe (oref slot type)))))
                               (hash-table-values (oref def slots)))))))
               (oref state defstructs))
              (maphash
               (lambda (name def)
                 (insert
                  (format
                   "%S\n"
                   `(elsa-declare-defclass ,name ,(oref def parents)
                      ,(mapcar (lambda (slot)
                                 (list (oref slot name)
                                       :type (read (elsa-type-describe (oref slot type)))))
                               (hash-table-values (oref def slots)))))))
               (oref state defclasses))
              (maphash
               (lambda (name def)
                 (insert
                  (format
                   "%S\n"
                   `(elsa-declare-defvar ,name ,(read (elsa-type-describe (oref def type)))))))
               (oref state defvars))
              (maphash
               (lambda (name def)
                 (when (oref def defgeneric-type)
                   (insert
                    (format
                     "%S\n"
                     `(elsa-declare-defgeneric ,name ,(when (slot-boundp def 'arglist) (oref def arglist))
                        ,(read (elsa-type-describe (oref def defgeneric-type)))))))
                 (insert
                  (format
                   "%S\n"
                   `(elsa-declare-defun ,name ,(when (slot-boundp def 'arglist) (oref def arglist))
                      ,(read (elsa-type-describe (oref def type)))))))
               (oref state defuns))
              (-each (nreverse (oref state requires))
                (lambda (req)
                  (insert (format ";; %S\n" `(elsa-load-cache ',req)))))
              (f-mkdir (f-parent elsa-cache-file))
              (f-write-text (buffer-string) 'utf-8 elsa-cache-file)
              (byte-compile-file elsa-cache-file))))))))

(defun elsa--processing-line (global-state operation file &optional worker-id duration)
  (pcase operation
    ("done"
     (with-ansi
      (green "[%s]" (elsa-global-state-get-counter global-state))
      (format " (worker %d) Processing file %s "
              worker-id
              (s-pad-right (+ 4 (oref global-state max-file-name-length))
                           " "
                           (concat file " ...")))
      (green "done")
      " after "
      (cond
       ((> duration 5)
        (bright-red "%.3fs" duration))
       ((> duration 2)
        (bright-yellow "%.3fs" duration))
       (t (green "%.3fs" duration)))))
    ("cache"
     (with-ansi
      (green "[%s]" (elsa-global-state-get-counter global-state))
      (format "            Processing file %s "
              (s-pad-right (+ 4 (oref global-state max-file-name-length))
                           " "
                           (concat file " ...")))
      (yellow "(loaded from cache)")))
    ("skipped"
     (with-ansi
      (green "[%s]" (elsa-global-state-get-counter global-state))
      (format "            Processing file %s "
              (s-pad-right (+ 4 (oref global-state max-file-name-length))
                           " "
                           (concat file " ...")))
      (bright-black "(skipped, already loaded)")))))

;; This is heavily inspired by `buttercup--debugger'.
(defun elsa--worker-debugger (&rest args)
  "Retrieve the backtrace from the point of error."
  (setq num-nonmacro-input-events (1+ num-nonmacro-input-events))
  (throw 'elsa-debugger-continue
         (list
          'failed
          (let ((frames nil)
                (i 0)
                (in-program-stack nil))
            (dolist (frame (backtrace-frames))
              (when in-program-stack
                (push frame frames))
              (when (eq (elt frame 1) 'elsa--worker-debugger)
                (setq in-program-stack t)))
            (concat
             (format "  %s\n" args)
             (mapconcat
              ;; Frame is EVALD FUNC ARGS FLAGS.  Flags is
              ;; useless so we drop it.
              (lambda (frame)
                (->>
                 (if (car frame)
                     (format "  %S%s"
                             (cadr frame)
                             (if (nth 2 frame)
                                 (cl-prin1-to-string (nth 2 frame))
                               "()"))
                   (format "  (%S %s)"
                           (cadr frame)
                           (mapconcat
                            (lambda (x) (format "%S" x))
                            (nth 2 frame)
                            " ")))
                 (replace-regexp-in-string "\n" "\\\\n")
                 (replace-regexp-in-string "%" "%%%%")))
              (nreverse frames)
              "\n"))))))

(defun elsa--worker-function-factory (worker-id project-directory)
  "Return function running in the Elsa analysis worker."
  (let ((load--path load-path))
    (lambda ()
      (setq load-path load--path)
      (require 'elsa)
      (-> (lgr-get-logger "elsa")
          (lgr-reset-appenders)
          (lgr-add-appender
           (-> (elsa-worker-appender)
               (lgr-set-layout (lgr-layout-format :format "[%g] %K %m"))))
          (lgr-set-threshold lgr-level-warn))
      (let ((result nil))
        (let ((lgr (lgr-get-logger "elsa.analyse.worker"))
              (msg nil)
              ;; this debugger stuff is used to reconstruct the
              ;; backtrace to the point of error
              (debugger #'elsa--worker-debugger)
              (debug-on-error t))
          (setq
           result
           (catch 'elsa-debugger-continue
             (setq elsa-is-language-server nil)
             (setq ansi-inhibit-ansi t)
             (setq elsa-global-state (elsa-global-state))
             (oset elsa-global-state project-directory project-directory)
             (oset elsa-global-state number-of-files 1)
             (oset elsa-global-state processed-file-index 1)
             (catch 'elsa-worker-done
               (while t
                 (setq msg (async-receive))
                 (let ((op (plist-get msg :op)))
                   (cond
                    ((equal op "analyze")
                     (let* ((dep (plist-get msg :dep))
                            (library (plist-get msg :file))
                            (current-time (current-time))
                            (elsa-cache-file (elsa--get-cache-file-name elsa-global-state dep)))
                       (elsa--autoload-types elsa-global-state dep)
                       (elsa--autoload-extension elsa-global-state dep)
                       (lgr-debug lgr "Starting analysis of %s" dep)
                       (let ((state (elsa-process-file library elsa-global-state)))
                         ;; `subr' has no provide for some circular
                         ;; dependency "bootstrap" issues.  We add it here
                         ;; artificially.
                         (lgr-debug lgr "Analysis of %s done" dep)
                         (when (equal dep "subr")
                           (oset state provide (list 'subr)))
                         (elsa-save-cache state elsa-global-state)
                         (lgr-debug lgr "Finished saving cache for %s" dep))
                       (async-send :ack "ok" :op op
                                   :dep dep
                                   :file library
                                   :duration (float-time
                                              (time-subtract
                                               (current-time) current-time)))))
                    ((equal op "load-from-cache")
                     (let ((files (plist-get msg :files)))
                       (dolist (file files) (load (f-no-ext file) t t))
                       (async-send :ack "ok" :op op)))
                    ((equal op "quit")
                     (lgr-warn lgr "Received signal to quit")
                     (throw 'elsa-worker-done t)))))))))
        (when (and (listp result)
                   (eq (car result) 'failed))
          (async-send :ack "error" :error (cadr result)))
        result))))

(defun elsa--parent-function-factory (worker-id workers-state max-file-name-length global-state)
  "Function handling child-to-parent messages and worker exit."
  (let ((lgr (lgr-get-logger "elsa.analyse.parent")))
    (lambda (result)
      (let ((worker-state (assoc worker-id workers-state)))
        (if (async-message-p result)
            (let* ((op (plist-get result :op))
                   (ack (plist-get result :ack)))
              (cond
               ((equal ack "error")
                (lgr-fatal lgr
                  (with-ansi
                   (red "Worker %d errored with:\n" worker-id)
                   (yellow (plist-get result :error))))
                (kill-emacs 1))
               ((equal op "echo")
                (lgr-debug lgr
                  (with-ansi
                   (red "Worker %d said: " worker-id)
                   (yellow (plist-get result :message)))))
               ((equal op "analyze")
                (let* ((dep (plist-get result :dep))
                       (file (plist-get result :file))
                       (duration (plist-get result :duration))
                       (elsa-cache-file (elsa--get-cache-file-name global-state dep)))
                  (elsa-log
                   (elsa--processing-line global-state "done" file worker-id duration))
                  (load (f-no-ext elsa-cache-file) t t))
                (cl-incf (oref global-state processed-file-index))
                (setf (cdr worker-state) (plist-put (cdr worker-state) :ready t)))
               ((equal op "load-from-cache")
                (lgr-debug lgr "Worker %s loaded all cache files for this layer" worker-id)
                (setf (cdr worker-state) (plist-put (cdr worker-state) :ready t)))))
          (lgr-debug lgr "Async process done in worker %d, result: %s" worker-id result)
          (setf (cdr worker-state) (plist-put (cdr worker-state) :ready t))
          t)))))

(defun elsa--wait-for-all (get-worker-states)
  (catch 'all-workers-ready
    (while t
      (when (--all? (plist-get (cdr it) :ready) (funcall get-worker-states))
        (throw 'all-workers-ready t))
      (sleep-for 0.01))))

(defun elsa-analyse-file-parallel (file global-state &optional already-loaded)
  "Analyse FILE with GLOBAL-STATE.

Optional argument ALREADY-LOADED is used to skip dependencies which
are already loaded in the currently running Emacs process.  This is
used by the LSP server to not reload already processed files."
  (elsa-with-elapsed-time "Process dependencies"
    (let* ((lgr (lgr-get-logger "elsa.analyse"))
           (dep-layers (elsa-with-elapsed-time "Resolving dependencies"
                         (elsa-get-dependencies-as-layers file)))
           (dependencies (let ((deps (append
                                      (plist-get dep-layers :layers)
                                      '(("simple"))
                                      '(("subr")))))
                           (unless (equal (caar deps) file)
                             (push (list file) deps))
                           (reverse deps)))
           (dep-to-file (plist-get dep-layers :visited))
           (max-file-name-length
            (if (plist-get dep-layers :visited)
                (->> (plist-get dep-layers :visited)
                     (--map (length (cdr it)))
                     (-max))
              0))
           (visited nil)
           (file-state nil)
           (worker-count (num-processors))
           ;; alist from worker ID to state
           (workers-state (--map (list it :ready t) (-iota worker-count)))
           (workers (--map (async-start
                            (elsa--worker-function-factory it (f-parent file))
                            (elsa--parent-function-factory
                             it
                             workers-state
                             max-file-name-length
                             global-state))
                           (-iota worker-count)))
           (i 0))
      (oset global-state project-directory (f-parent file))
      (oset global-state processed-file-index 1)
      (oset global-state number-of-files (length (-flatten dependencies)))
      (oset global-state max-file-name-length max-file-name-length)
      (lgr-debug lgr "Processing dependency layers: %s" dependencies)
      (lgr-debug lgr "Processing dependency alist: %s" dep-to-file)
      (dolist (layer (butlast dependencies))
        (cl-incf i)
        (lgr-debug lgr "Processing layer %s" i)
        (while layer
          (-each workers-state
            (-lambda ((state &as worker-id . (&plist :ready)))
              (when (and ready layer)
                (lgr-debug lgr "Worker %s is ready, submitting dependency %s" worker-id (car layer))
                (let* ((worker (nth worker-id workers))
                       (dep (pop layer))
                       (file (or (cdr (assoc dep dep-to-file))
                                 (elsa--find-dependency dep)))
                       (elsa-cache-file (elsa--get-cache-file-name global-state dep)))
                  (cond
                   ((not file)
                    (elsa-log (with-ansi (red "Failed to find dependency %s" dep))))
                   ((and (or (member file visited)
                             (member file already-loaded))
                         (not (file-newer-than-file-p file elsa-cache-file)))
                    (elsa-log (elsa--processing-line global-state "skipped" file))
                    (cl-incf (oref global-state processed-file-index)))
                   ((file-newer-than-file-p elsa-cache-file file)
                    (push file visited)
                    (load (f-no-ext elsa-cache-file) t t)
                    (elsa-log (elsa--processing-line global-state "cache" file))
                    (cl-incf (oref global-state processed-file-index)))
                   (t
                    (push file visited)
                    (elsa--autoload-types global-state dep)
                    (elsa--autoload-extension global-state dep)
                    (setf (cdr state) (plist-put (cdr state) :ready nil))
                    (async-send worker :op "analyze" :dep dep :file file)))))))
          (sleep-for 0.01))
        (lgr-debug lgr "All work for layer %s was distributed" i)

        (elsa--wait-for-all (lambda () workers-state))
        (lgr-debug lgr "All workers finished processing layer %s" i)

        (let ((cache-files (mapcar
                            (lambda (dep)
                              (elsa--get-cache-file-name global-state dep))
                            layer)))
          (-each workers-state
            (-lambda ((state &as worker-id))
              (setf (cdr state) (plist-put (cdr state) :ready nil))
              (let ((worker (nth worker-id workers)))
                (async-send worker :op "load-from-cache" :files cache-files)))))

        (elsa--wait-for-all (lambda () workers-state))
        (lgr-debug lgr "All workers updated global state for layer %s" i))

      (-each workers-state
        (-lambda ((state &as worker-id))
          (setf (cdr state) (plist-put (cdr state) :ready nil))
          (let ((worker (nth worker-id workers)))
            (async-send worker :op "quit"))))
      (elsa--wait-for-all (lambda () workers-state))
      (lgr-debug lgr "All workers quit")

      (let ((start-time (current-time))
            (state (elsa-process-file (car (-last-item dependencies)) global-state 'no-log))
            (file-fullname (file-truename file)))
        (push file-fullname visited)
        (oset state dependencies visited)
        (elsa-log
         (elsa--processing-line global-state "done" file-fullname -1
                                (elsa-get-elapsed start-time)))
        (cl-incf (oref global-state processed-file-index))
        (elsa-save-cache state global-state)
        (elsa-state-update-global state global-state)
        state))))

(defun elsa-analyse-file (file global-state &optional already-loaded)
  "Analyse FILE with GLOBAL-STATE.

Optional argument ALREADY-LOADED is used to skip dependencies which
are already loaded in the currently running Emacs process.  This is
used by the LSP server to not reload already processed files.

This function is soft-deprecated in favour of
`elsa-analyse-file-parallel'."
  (let ((dependencies (cons
                       "subr" ; subr is always a dependency
                       (elsa-get-dependencies file)))
        (visited nil)
        (file-state nil))
    (oset global-state project-directory (f-parent file))
    ;; (elsa-log "Processing transitive dependencies: %s" (s-join ", " dependencies))
    (oset global-state processed-file-index 1)
    (oset global-state number-of-files (length dependencies))
    (dolist (dep dependencies)
      (let* ((dep (replace-regexp-in-string "-CIRCULAR\\'" "" dep))
             (elsa-cache-file (elsa--get-cache-file-name global-state dep)))
        (when-let* ((library (file-truename
                              (if (f-exists? dep) dep
                                (elsa--find-dependency dep)))))
          (if (and (or (member library visited)
                       (member library already-loaded))
                   (not (file-newer-than-file-p library elsa-cache-file)))
              (elsa-log
               (with-ansi
                (green "[%s]" (elsa-global-state-get-counter global-state))
                (format " Processing file %s ... " library)
                (bright-black "(skipped, already loaded)")))
            (push library visited)
            (elsa--autoload-types elsa-global-state dep)
            (elsa--autoload-extension elsa-global-state dep)
            (if (file-newer-than-file-p library elsa-cache-file)
                (let ((state (elsa-process-file library global-state)))
                  ;; `subr' has no provide for some circular
                  ;; dependency "bootstrap" issues.  We add it here
                  ;; artificially.
                  (when (equal dep "subr")
                    (oset state provide (list 'subr)))
                  (elsa-save-cache state global-state)
                  ;; copy new definitions to the global state
                  (elsa-state-update-global state global-state)
                  (setq file-state state))
              (elsa-log
               (with-ansi
                (green "[%s]" (elsa-global-state-get-counter global-state))
                (format " Processing file %s ... " library)
                (yellow "(loaded from cache %s)" elsa-cache-file)))
              (load (f-no-ext elsa-cache-file) t t)))))
      (cl-incf (oref global-state processed-file-index)))
    (oset file-state dependencies visited)
    file-state))

(defun elsa-process-form (&optional state)
  "Read and analyse form at point."
  (interactive)
  (let* ((state (or state (elsa-state :global-state elsa-global-state))))
    (when-let ((form (elsa-read-form state)))
      (oset (oref state global-state) current-file (buffer-file-name))
      (elsa-analyse-form state form)
      (elsa-state-update-global state (oref state global-state))
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
          (progn
            (goto-char (point-min))
            (while (setq form (read (current-buffer)))
              (pcase form
                (`(register-extensions . ,extensions)
                 (--each extensions
                   (if (require (intern (concat "elsa-extension-" (symbol-name it))) nil t)
                       (elsa-log "Loaded extension for %s" (symbol-name it))
                     (princ (format "An error occured during startup: Extension %s not found\n" (symbol-name it))))))
                (`(register-ruleset . ,rulesets)
                 (--each rulesets
                   (let ((ruleset-constructor (intern (concat "elsa-ruleset-" (symbol-name it)))))
                     (if (functionp ruleset-constructor)
                         (progn
                           (elsa-ruleset-load (funcall ruleset-constructor))
                           (elsa-log "Loaded ruleset %s" (symbol-name it)))
                       (princ (format "An error occured during startup: Ruleset %s not found\n" (symbol-name it))))))))))
        (end-of-file t)))))

(defvar elsa-cli-with-exit nil)
(defun elsa--cli-with-exit ()
  (setq elsa-cli-with-exit t))

(defun elsa-run ()
  "Analyze files and output errors to stdout.

Elsa supports some optional flags when executed from command
line.  These must come before the list of files to be analyzed.
Currently, these flags are supported:

-with-exit => Sets the process exit code to non zero if errors
              were reported.  This flag exists because flycheck
              complains when process exits with non-zero status."
  (elsa-load-config)

  (require 'elsa-startup)

  (let ((errors 0)
        (warnings 0)
        (notices 0)
        (current-time (current-time)))

    ;; Process command line options
    (catch 'done
      (while t
        (let ((opt (car command-line-args-left)))
          (cond
           ((equal opt "-with-exit")
            (elsa--cli-with-exit)
            (pop command-line-args-left))
           (t (throw 'done t))))))

    (dolist (file command-line-args-left)
      (--each (reverse (oref (elsa-analyse-file-parallel
                              file elsa-global-state)
                             errors))
        (cond
         ((elsa-error-p it) (cl-incf errors))
         ((elsa-warning-p it) (cl-incf warnings))
         ((elsa-notice-p it) (cl-incf notices)))
        (elsa-log (with-ansi (yellow file) ":" (elsa-message-format it)))))
    (let ((duration (float-time
                     (time-subtract
                      (current-time) current-time))))
      (elsa-log (with-ansi
                 "\nAnalysis finished with "
                 (bright-red "%d errors" errors)
                 ", "
                 (bright-yellow "%d warnings" warnings)
                 " and "
                 (bright-blue "%d notices" notices)
                 " after "
                 (blue "%.3f seconds" duration)))

      (lgr-trace (lgr-get-logger "elsa.perf")
        "memory report %s"
        (with-current-buffer (get-buffer-create "*Memory Report*")
          (require 'memory-report)
          (memory-report)
          (buffer-string))))
    (when (and elsa-cli-with-exit (< 0 errors))
      (kill-emacs 1))))

(provide 'elsa)
;;; elsa.el ends here
