(require 'json)
(require 'files)

(require 'lsp-mode)

(require 'elsa)

(defun append-to-log-file (message)
  "Appends a message to a log file."
  (let ((log-file "elsa-lsp.log"))
    (with-temp-buffer
      (insert (replace-regexp-in-string "\\(\r\\|\n\\)" "" message) "\n")
      (write-region (point-min) (point-max) log-file t))))

(defun elsa-lsp-send-response (message)
  (when (or (hash-table-p message)
            (and (listp message) (plist-get message :jsonrpc)))
    (setq message (lsp--json-serialize message)))

  ;; (append-to-log-file (concat "<< " message))
  (princ (format "Content-Length: %d\r\n\r\n" (string-bytes message)))
  (princ message)
  (terpri))

(defclass elsa-lsp-appender (lgr-appender) ()
  "Appender sending messages to lsp client as window/showMessage.")

(cl-defmethod lgr-append ((this elsa-lsp-appender) event)
  "Send window/showMessage LSP notification."
  (when elsa-is-language-server
    (elsa-lsp-send-response
     (lsp--make-notification
      "window/showMessage"
      (lsp-make-message-params
       :type lsp/message-type-info
       :message (lgr-format-event (oref this layout) event)))))
  this)

(defconst elsa-lsp-logger (-> (lgr-get-logger "elsa")
                              (lgr-add-appender
                               (-> (elsa-lsp-appender)
                                   (lgr-set-layout (elsa-plain-layout))
                                   (lgr-set-threshold lgr-level-info)))))

(defclass elsa-lsp-file ()
  ((name :type string :initarg :name)
   (buffer :type buffer :initarg :buffer)))

(defclass elsa-lsp-state ()
  ((files
    :type hash-table :initform (make-hash-table :test #'equal)
    :documentation "Workspace files.")
   (dependencies
    :type list :initform nil
    :documentation "Loaded dependencies.

We use this list to remove dependencies which don't need to
be re-analysed during textDocument/didOpen handler.")))

(defvar elsa-lsp-state (elsa-lsp-state))

(cl-defmethod elsa-lsp-add-file ((state elsa-lsp-state) (file string))
  (message "Added file %s to state" file)
  (puthash file
           (elsa-lsp-file
            :name file
            :buffer (with-current-buffer
                        (get-buffer-create (concat "elsa-lsp-" file))
                      (emacs-lisp-mode)
                      (current-buffer)))
           (oref state files))
  (elsa-lsp-update-file-buffer state file))

(cl-defmethod elsa-lsp-get-file ((state elsa-lsp-state) (file string))
  (gethash file (oref state files)))

(cl-defmethod elsa-lsp-get-buffer ((state elsa-lsp-state) (file string))
  (when-let ((elsa-file (elsa-lsp-get-file state file)))
    (oref elsa-file buffer)))

(cl-defmethod elsa-lsp-update-file-buffer ((state elsa-lsp-state) (file string) &optional content)
  (message "Trying to update file %s" file)
  (when-let ((elsa-buffer (elsa-lsp-get-buffer state file)))
    (with-current-buffer elsa-buffer
      (erase-buffer)
      (if (and content (stringp content))
          (insert content)
        (insert-file-contents file)))
    (message "Updated file %s" file)))

(cl-defmethod elsa-lsp--analyze-file ((this elsa-lsp-state) (file string))
  (let ((state (elsa-analyse-file-parallel
                file
                elsa-global-state
                (oref this dependencies))))
    (oset this dependencies
          (-uniq
           (-concat
            (oref this dependencies)
            (oref state dependencies))))
    state))

(defun elsa-lsp-register ()
  (interactive)
  (add-to-list 'lsp-language-id-configuration '(emacs-lisp-mode . "emacs-lisp"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       (cond
                        ((locate-dominating-file (buffer-file-name) "Eask")
                         (list "eask" "exec" "elsa-lsp"))
                        ((locate-dominating-file (buffer-file-name) "Cask")
                         (list "cask" "exec" "elsa-lsp"))
                        (t (error "Elsa Language Server can only run with Eask or Cask")))))
    :major-modes '(emacs-lisp-mode)
    :priority 1
    :server-id 'elsa)))

(defun elsa-lsp--uri-to-file (uri)
  (substring uri 7))

(defun elsa-lsp--completions-bounds ()
  (with-syntax-table emacs-lisp-mode-syntax-table
    (message "completion bounds point %s" (point))
    (let* ((pos (point))
           (beg (condition-case nil
                    (save-excursion
                      (backward-sexp 1)
                      (skip-chars-forward "`',‘#")
                      (point))
                  (scan-error pos)))
           (end
            (unless (or (eq beg (point-max))
                        (member (char-syntax (char-after beg))
                                '(?\" ?\()))
              (condition-case nil
                  (save-excursion
                    (goto-char beg)
                    (forward-sexp 1)
                    (skip-chars-backward "'’")
                    (when (>= (point) pos)
                      (point)))
                (scan-error pos)))))
      (list beg end))))

(defun elsa-lsp--list-completion-items (list &optional transform)
  (setq transform (or transform #'identity))
  (apply #'vector
         (mapcar (lambda (item)
                   (lsp-make-completion-item
                    :label (funcall transform item) ))
                 list)))

(defun elsa-lsp--completions ()
  ;; only used to extract start and end... we can reimplement it later
  (-when-let ((beg end) (elsa-lsp--completions-bounds))
    (message "bounds %s %s" beg end)
    (let* ((candidates nil)
           (funpos (eq (char-before beg) ?\())
           (prefix (buffer-substring-no-properties beg end)))
      (message "prefix %s" prefix)
      (cond
       (funpos
        (maphash
         (lambda (k v)
           (when (string-prefix-p prefix (symbol-name k))
             (push k candidates)))
         (oref elsa-global-state defuns))
        (message "has %d functions for prefix %s" (length candidates) prefix)))
      (mapcar #'symbol-name candidates))))

(defun elsa-lsp--capf-completions ()
  "Fallback completions engine is the `elisp-completion-at-point'."
  (-when-let ((start end table . props) (elisp-completion-at-point))
    (-let* ((predicate (plist-get props :predicate))
            (prefix (buffer-substring-no-properties start end))
            (meta (completion-metadata prefix table predicate))
            (candidates (completion-all-completions
                         prefix table predicate (length prefix) meta))
            (last (last candidates))
            (base-size (and (numberp (cdr last)) (cdr last))))
      (when base-size
        (setcdr last nil))
      candidates)))

(defun elsa-lsp--get-diagnostics (file)
  (let ((state (progn
                 (oset elsa-global-state number-of-files 1)
                 (oset elsa-global-state processed-file-index 1)
                 (elsa-process-file file elsa-global-state))))
    (elsa-state-update-global state elsa-global-state)
    (apply #'vector (mapcar #'elsa-message-to-lsp (oref state errors)))))

(defun elsa-lsp--handle-textDocument/hover (id method params)
  (-let* (((&HoverParams :text-document (&TextDocumentIdentifier :uri)
                         :position (&Position :line :character))
           params)
          (file (elsa-lsp--uri-to-file uri))
          (buffer (elsa-lsp-get-buffer elsa-lsp-state file)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line line)
        (forward-char character)
        (beginning-of-defun)
        (let* ((state (elsa-state :global-state elsa-global-state
                                  :lsp-params params
                                  :lsp-method method))
               (value (catch 'lsp-response
                        (elsa-process-form state))))
          (when (and value (hash-table-p value))
            (lsp--make-response id value)))))))

(defun elsa-lsp--handle-textDocument/didChange (id method params)
  (-let* (((&DidChangeTextDocumentParams
            :text-document (&VersionedTextDocumentIdentifier :uri :version?)
            :content-changes [(&TextDocumentContentChangeEvent :text)])
           params)
          (file (elsa-lsp--uri-to-file uri))
          (buffer (elsa-lsp-get-buffer elsa-lsp-state file)))
    (elsa-lsp-update-file-buffer elsa-lsp-state file text)

    ;; load new dependencies to LSP server process
    (when buffer
      (let ((deps (elsa--get-requires buffer))
            (loaded-deps (oref elsa-lsp-state dependencies))
            (to-process nil))
        (message "dependencies found in file %s: %s" file deps)
        (-each deps
          (lambda (dep)
            (unless (member (car dep) loaded-deps)
              (push (car dep) to-process))))
        (when to-process
          (message "new dependencies to process: %s" to-process)
          (-each to-process
            (lambda (new-dep)
              (elsa-lsp--analyze-file elsa-lsp-state new-dep))))))
    nil))

(defun elsa-lsp--on-request (id method params)
  (message ">> %s" (lsp--json-serialize (list :id id :method method :params params)))
  (let ((res (cond
              ((equal method "initialize")
               (lsp--make-response
                id
                (lsp-make-initialize-result
                 :server-info (lsp-make-server-info
                               :name "elsa-lsp"
                               :version? "0.0.1")
                 :capabilities (lsp-make-server-capabilities
                                :hover-provider? t
                                :text-document-sync? (lsp-make-text-document-sync-options
                                                      :open-close? t
                                                      :save? t
                                                      :change 1)
                                :completion-provider? (lsp-make-completion-options
                                                       :resolve-provider? json-false
                                                       :trigger-characters? [":" "-"])))))
              ((equal method "textDocument/hover")
               (elsa-lsp--handle-textDocument/hover id method params))
              ((equal method "textDocument/completion")
               (-let* (((&CompletionParams :text-document (&TextDocumentIdentifier :uri)
                                           :position (&Position :line :character))
                        params)
                       (file (elsa-lsp--uri-to-file uri))
                       (buffer (elsa-lsp-get-buffer elsa-lsp-state file)))
                 (when buffer
                   (with-current-buffer buffer
                     (goto-char (point-min))
                     (forward-line line)
                     (forward-char character)
                     (let* ((state (elsa-state :global-state elsa-global-state
                                               :lsp-params params
                                               :lsp-method method))
                            (value (ignore-errors
                                     (catch 'lsp-response
                                       (save-excursion
                                         (beginning-of-defun)
                                         (elsa-process-form state))))))
                       (if (and value (hash-table-p value))
                           (lsp--make-response id value)
                         ;; fall back to capf
                         (lsp--make-response
                          id
                          (lsp-make-completion-list
                           :is-incomplete json-false
                           :items (apply
                                   #'vector
                                   (mapcar
                                    (lambda (item)
                                      (lsp-make-completion-item :label item))
                                    (elsa-lsp--capf-completions)))))))))))
              ((equal method "textDocument/didOpen")
               (-let* (((&DidOpenTextDocumentParams :text-document (&TextDocumentItem :uri :version)) params)
                       (file (elsa-lsp--uri-to-file uri)))
                 (elsa-lsp-add-file elsa-lsp-state file)
                 (let ((state (elsa-lsp--analyze-file elsa-lsp-state file)))
                   (lsp--make-notification
                    "textDocument/publishDiagnostics"
                    (lsp-make-publish-diagnostics-params
                     :uri uri
                     :version version
                     :diagnostics (apply
                                   #'vector
                                   (mapcar #'elsa-message-to-lsp
                                           (oref state errors))))))))
              ((equal method "textDocument/didSave")
               (-let* (((&DidSaveTextDocumentParams :text-document (&TextDocumentItem :uri :version)) params)
                       (file (elsa-lsp--uri-to-file uri)))
                 (elsa-lsp-update-file-buffer elsa-lsp-state file)
                 (lsp--make-notification
                  "textDocument/publishDiagnostics"
                  (lsp-make-publish-diagnostics-params
                   :uri uri
                   :version version
                   :diagnostics (elsa-lsp--get-diagnostics file)))))
              ((equal method "textDocument/didChange")
               (elsa-lsp--handle-textDocument/didChange id method params)))))
    (if (not res)
        (message "<< %s" "no response")
      (message "<< %s" (lsp--json-serialize res))
      (elsa-lsp-send-response (lsp--json-serialize res)))))

(provide 'elsa-lsp-core)
