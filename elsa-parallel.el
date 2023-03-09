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
                (let* ((dep (plist-get msg :file))
                       (library (file-truename
                                 (if (f-exists? dep) dep
                                   (elsa--find-dependency dep))))
                       (current-time (current-time)))
                  (when library
                    (condition-case err
                        (let ((state (elsa-process-file library elsa-global-state)))
                          ;; `subr' has no provide for some circular
                          ;; dependency "bootstrap" issues.  We add it here
                          ;; artificially.
                          (when (equal dep "subr")
                            (oset state provide (list 'subr)))
                          ;; (elsa-save-cache state elsa-global-state)
                          )
                      (error (async-send :ack "error" :error err))))
                  (async-send :ack "ok" :op op
                              :worker-id worker-id :file dep
                              :duration (float-time
                                         (time-subtract
                                          (current-time) current-time)))))
               ((equal op "load-from-cache")
                (let ((files (plist-get msg :files)))
                  (dolist (file files) (load (f-no-ext file) t t))
                  (async-send :ack "ok" :op op :worker-id worker-id)))
               ((equal op "quit")
                (throw 'done t))))))))))

(defun elsa--parent-function-factory (worker-id workers-state global-state)
  "Function handling child-to-parent messages and worker exit."
  (lambda (result)
    (if (async-message-p result)
        (let* ((worker-id (plist-get result :worker-id))
               (worker-state (assoc worker-id workers-state))
               (op (plist-get result :op)))
          (cond
           ((equal op "analyze")
            (let ((file (plist-get result :file))
                  (duration (plist-get result :duration)))
              (elsa-log
               (with-ansi
                (green "[%s]" (elsa-global-state-get-counter global-state))
                (format " (worker %d) Processing file %s ... " worker-id file)
                (green "done")
                " after "
                (cond
                 ((> duration 5)
                  (bright-red "%.3fs" duration))
                 ((> duration 2)
                  (bright-yellow "%.3fs" duration))
                 (t (green "%.3fs" duration))))))
            (cl-incf (oref global-state processed-file-index))
            (setf (cdr worker-state) (plist-put (cdr worker-state) :ready t)))
           ((equal op "load-from-cache")
            ;; (message "Worker %s loaded all cache files for this layer" worker-id)
            (setf (cdr worker-state) (plist-put (cdr worker-state) :ready t)))))
      ;; (message "Async process done in worker %d, result: %s" worker-id result)
      t
      )))


(defun elsa--wait-for-all (get-worker-states)
  (catch 'all-workers-ready
    (while t
      (when (--all? (plist-get (cdr it) :ready) (funcall get-worker-states))
        (throw 'all-workers-ready t))
      (sleep-for 0.2))))

(defun elsa-analyse-file-parallel (file global-state &optional already-loaded)
  "Analyse FILE with GLOBAL-STATE.

Optional argument ALREADY-LOADED is used to skip dependencies which
are already loaded in the currently running Emacs process.  This is
used by the LSP server to not reload already processed files."
  (my-with-elapsed-timer "process dependencies"
    (let* ((dependencies (reverse
                          (append
                           (my-with-elapsed-timer "resolving dependencies"
                             (elsa-get-dependencies-as-layers file))
                           '(("subr")))))
           (visited nil)
           (file-state nil)
           ;; alist from worker ID to state
           (workers-state (--map (list it :ready t) (-iota 5)))
           (workers (--map (async-start
                            (elsa--worker-function-factory it (f-parent file))
                            (elsa--parent-function-factory it workers-state global-state)
                            ;;(lambda (result) (message "result %s" result))
                            )
                           (-iota 5)))
           (i 0))
      (oset global-state project-directory (f-parent file))
      (oset global-state processed-file-index 1)
      (oset global-state number-of-files (length (-flatten dependencies)))
      (elsa-log "Processing dependency layers: %s" dependencies)
      (dolist (layer dependencies)
        (cl-incf i)
        ;; (elsa-log "processing layer %s" i)
        (while layer
          (-each workers-state
            (-lambda ((state &as worker-id . (&plist :ready)))
              (when (and ready layer)
                ;(elsa-log "Worker %s is ready, submitting dependency %s" worker-id (car layer))
                (setf (cdr state) (plist-put (cdr state) :ready nil))
                (let ((worker (nth worker-id workers)))
                  (async-send worker :op "analyze" :file (pop layer))))))
          (sleep-for 0.2))
        ;(elsa-log "All work for layer %s was distributed" i)
        (elsa--wait-for-all (lambda () workers-state))
        ;; (catch 'all-workers-ready
        ;;   (while t
        ;;     (when (--all? (plist-get (cdr it) :ready) workers-state)
        ;;       (throw 'all-workers-ready t))
        ;;     (sleep-for 0.2)))
        ;(elsa-log "All workers finished processing layer %s" i)
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
        ;(elsa-log "All workers updated global state for layer %s" i)
        )
      (--each workers
        (async-send it :op "quit")))))

(provide 'elsa-parallel)
;;; elsa-parallel.el ends here
