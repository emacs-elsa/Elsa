;; -*- lexical-binding: t -*-

;;    (elsa-log :: (function (string &rest mixed) string))
(defun elsa-log (fmt &rest args)
  (princ (apply #'format (concat fmt "\n") args)))

(provide 'elsa-log)
