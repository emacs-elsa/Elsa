(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))

(require 'elsa-lsp-core)
(setq elsa-is-language-server t)

(defun elsa-lsp-stdin-loop ()
  "Reads from standard input in a loop and process incoming requests."
  (elsa-load-config)
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
            (error (elsa-log "Elsa lsp error: %s" (error-message-string err)))))))

      (setq input (read-from-minibuffer "")))))

(provide 'elsa-lsp)
