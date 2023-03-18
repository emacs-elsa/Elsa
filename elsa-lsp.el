(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))

(require 'elsa-lsp-core)
(setq elsa-is-language-server t)

(defun elsa-lsp-stdin-loop ()
  "Reads from standard input in a loop and process incoming requests."
  (elsa-load-config)
  (require 'elsa-startup)

  (-> (lgr-get-logger "elsa")
      (lgr-reset-appenders)
      (lgr-add-appender
       (-> (elsa-lsp-appender)
           (lgr-set-layout (elsa-plain-layout))
           (lgr-set-threshold lgr-level-info)))
      (lgr-add-appender
       (-> (lgr-appender)
           (lgr-set-layout (lgr-layout-format :format "[%K] (%n) %m"))))
      (lgr-set-threshold lgr-level-debug))

  (prettify-symbols-mode -1)
  (let ((input (read-from-minibuffer ""))
        (has-header nil))
    (while input
      ;; (append-to-log-file (concat ">> " input))
      (cond
       ((equal input ""))
       ((string-match-p (rx "content-length: " (group (1+ digit))) input)
        (setq has-header t))
       (has-header
        (-let* (((&JSONResponse :params :method :id) (lsp--read-json input)))
          (condition-case err
              (elsa-lsp--on-request id method params)
            (error (lgr-error (lgr-get-logger "elsa")
                     "Elsa lsp error: %s"
                     (error-message-string err)))))))

      (setq input (read-from-minibuffer "")))))

(provide 'elsa-lsp)
