;; -*- lexical-binding: t -*-

(defconst elsa-log-level-critical 1)
(defconst elsa-log-level-info 2)
(defconst elsa-log-level-debug 3)
(defconst elsa-log-level-trace 4)

(defvar elsa-log-level 2)

;;    (elsa-log :: (function (string &rest mixed) string))
(defun elsa-log (fmt &rest args)
  (let ((msg (apply #'format fmt args)))
    (when elsa-is-language-server
      (elsa-lsp-send-response
       (lsp--make-notification
        "window/showMessage"
        (lsp-make-message-params :type 3 :message msg))))
    (princ (concat msg "\n"))))

(defun elsa-debug (fmt &rest args)
  (when (<= elsa-log-level-debug elsa-log-level)
    (apply #'elsa-log fmt args)))

(defun elsa-get-elapsed (start)
  "Get time elapsed since START in floating point seconds."
  (float-time (time-subtract (current-time) start)))

(defmacro elsa-with-elapsed-time (msg &rest body)
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (elapsedvar (make-symbol "elapsed")))
    `(let ((,nowvar (current-time)))
       (elsa-log "%s..." ,msg)
       (prog1 (progn ,@body)
         (let ((,elapsedvar
                (float-time (time-subtract (current-time) ,nowvar))))
           (elsa-log "%s...done (%.3fs)" ,msg ,elapsedvar))))))

(provide 'elsa-log)
