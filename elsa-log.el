;; -*- lexical-binding: t -*-

;;    (elsa-log :: (function (string &rest mixed) string))
(defun elsa-log (fmt &rest args)
  (let ((msg (apply #'format fmt args)))
    (when elsa-is-language-server
      (elsa-lsp-send-response
       (lsp--make-notification
        "window/showMessage"
        (lsp-make-message-params :type 3 :message msg))))
    (princ (concat msg "\n"))))

(provide 'elsa-log)
