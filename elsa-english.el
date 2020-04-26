;; (elsa-pluralize :: (function (string int) string))
(defun elsa-pluralize (word n)
  "Return singular or plural of WORD based on N."
  (if (= n 1)
      word
    (concat word "s")))

(provide 'elsa-english)
