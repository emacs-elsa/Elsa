;;; elsa-parallel.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created:  9th March 2023
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

(require 'async)

(defun elsa--autoload-types (global-state dep)
  (when (require (intern (concat "elsa-typed-"
                                 (replace-regexp-in-string ".el\\'" "" dep)))
                 nil t)
    (elsa-log "%sAutoloading types for %s"
              (make-string (elsa-global-state-prefix-length global-state 3) ? )
              dep)))

(defun elsa--autoload-extension (global-state dep)
  (when (require (intern (concat "elsa-extension-"
                                 (replace-regexp-in-string ".el\\'" "" dep)))
                 nil t)
    (elsa-log "%sAutoloading extension for %s"
              (make-string (elsa-global-state-prefix-length global-state 3) ? )
              dep)))

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

(defun elsa--worker-function-factory (worker-id project-directory)
  "Return function running in the Elsa analysis worker."
  (let ((load--path load-path))
    (lambda ()
      (setq load-path load--path)
      (setq elsa-is-language-server nil)
      (setq ansi-inhibit-ansi t)
      (require 'elsa)
      (require 'async)
      (let ((msg nil))
        (setq elsa-global-state (elsa-global-state))
        (oset elsa-global-state project-directory project-directory)
        (oset elsa-global-state number-of-files 1)
        (oset elsa-global-state processed-file-index 1)
        (catch 'done
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
                  (condition-case err
                      (let ((state (elsa-process-file library elsa-global-state)))
                        ;; `subr' has no provide for some circular
                        ;; dependency "bootstrap" issues.  We add it here
                        ;; artificially.
                        (when (equal dep "subr")
                          (oset state provide (list 'subr)))
                        (elsa-save-cache state elsa-global-state))
                    (error (async-send :ack "error" :error err)))
                  (async-send :ack "ok" :op op
                              :worker-id worker-id :dep dep
                              :file library
                              :duration (float-time
                                         (time-subtract
                                          (current-time) current-time)))))
               ((equal op "load-from-cache")
                (let ((files (plist-get msg :files)))
                  (dolist (file files) (load (f-no-ext file) t t))
                  (async-send :ack "ok" :op op :worker-id worker-id)))
               ((equal op "quit")
                (throw 'done t))))))))))

(defun elsa--parent-function-factory (worker-id workers-state max-file-name-length global-state)
  "Function handling child-to-parent messages and worker exit."
  (lambda (result)
    (if (async-message-p result)
        (let* ((worker-id (plist-get result :worker-id))
               (worker-state (assoc worker-id workers-state))
               (op (plist-get result :op)))
          (cond
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
            (elsa-debug "Worker %s loaded all cache files for this layer" worker-id)
            (setf (cdr worker-state) (plist-put (cdr worker-state) :ready t)))))
      (elsa-debug "Async process done in worker %d, result: %s" worker-id result)
      t)))

(defun elsa--wait-for-all (get-worker-states)
  (catch 'all-workers-ready
    (while t
      (when (--all? (plist-get (cdr it) :ready) (funcall get-worker-states))
        (throw 'all-workers-ready t))
      (sleep-for 0.1))))

(defun elsa-analyse-file-parallel (file global-state &optional already-loaded)
  "Analyse FILE with GLOBAL-STATE.

Optional argument ALREADY-LOADED is used to skip dependencies which
are already loaded in the currently running Emacs process.  This is
used by the LSP server to not reload already processed files."
  (elsa-with-elapsed-time "Process dependencies"
    (let* ((dep-layers (elsa-with-elapsed-time "Resolving dependencies"
                         (elsa-get-dependencies-as-layers file)))
           (dependencies (let ((deps (append
                                      (plist-get dep-layers :layers)
                                      '(("subr")))))
                           (unless (equal (caar deps) file)
                             (push (list file) deps))
                           (reverse deps)))
           (dep-to-file (plist-get dep-layers :visited))
           (max-file-name-length
            (->> (plist-get dep-layers :visited)
                 (--map (length (cdr it)))
                 (-max)))
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
      (elsa-debug "Processing dependency layers: %s" dependencies)
      (elsa-debug "Processing dependency alist: %s" dep-to-file)
      (dolist (layer (butlast dependencies))
        (cl-incf i)
        (elsa-debug "Processing layer %s" i)
        (while layer
          (-each workers-state
            (-lambda ((state &as worker-id . (&plist :ready)))
              (when (and ready layer)
                (elsa-debug "Worker %s is ready, submitting dependency %s" worker-id (car layer))
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
          (sleep-for 0.1))
        (elsa-debug "All work for layer %s was distributed" i)

        (elsa--wait-for-all (lambda () workers-state))
        (elsa-debug "All workers finished processing layer %s" i)

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
        (elsa-debug "All workers updated global state for layer %s" i))

      (--each workers
        (async-send it :op "quit"))
      (elsa-debug "All workers quit")

      (let ((start-time (current-time))
            (state (elsa-process-file (car (-last-item dependencies)) global-state 'no-log))
            (file-fullname (file-truename file)))
        (push file-fullname visited)
        (oset state dependencies visited)
        (elsa-log
         (elsa--processing-line global-state "done" file-fullname -1
                                (elsa-get-elapsed start-time)))
        (cl-incf (oref global-state processed-file-index))
        state))))

(provide 'elsa-parallel)
;;; elsa-parallel.el ends here
