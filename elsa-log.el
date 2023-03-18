;; -*- lexical-binding: t -*-

(require 'async)
(require 'dash)

(require 'lgr)

(defclass elsa-worker-appender (lgr-appender) ()
  "Appender sending messages back to parent process.")

(cl-defmethod lgr-append ((this elsa-worker-appender) event)
  "Send messages to parent process with `async-send'."
  (when async-in-child-emacs
    (async-send
     :op "echo"
     :message (lgr-format-event (oref this layout) event)))
  this)

(defclass elsa-plain-layout (lgr-layout) ()
  "Simple layout only printing the message.")

(cl-defgeneric lgr-format-event ((_this elsa-plain-layout) (event lgr-event))
  "Format EVENT as only the message."
  (oref event msg))

(defconst elsa-logger (-> (lgr-get-logger "elsa")
                          (lgr-reset-appenders)
                          (lgr-add-appender
                           (-> (lgr-appender-princ)
                               (lgr-set-layout (elsa-plain-layout))))
                          (lgr-set-threshold lgr-level-info))
  "Configuration for main elsa logger.")

;; (elsa-log :: (function (string &rest mixed) string))
(defun elsa-log (fmt &rest args)
  "Log messages at info level via `elsa-logger'."
  (apply #'lgr-log elsa-logger 400 fmt args))

(defun elsa-get-elapsed (start)
  "Get time elapsed since START in floating point seconds."
  (float-time (time-subtract (current-time) start)))

(defmacro elsa-with-elapsed-time (msg &rest body)
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (elapsedvar (make-symbol "elapsed")))
    `(let ((,nowvar (current-time)))
       (lgr-info elsa-logger "%s..." ,msg)
       (prog1 (progn ,@body)
         (let ((,elapsedvar
                (float-time (time-subtract (current-time) ,nowvar))))
           (lgr-info elsa-logger "%s...done (%.3fs)" ,msg ,elapsedvar))))))

(provide 'elsa-log)
