(require 'elsa-analyser)
(require 'elsa-typed-eieio)

(defun elsa--analyse:oref (form scope state)
  (elsa--analyse-macro form (list t nil) scope state))

(defun elsa--analyse:oset (form scope state)
  (elsa--analyse-macro form (list t nil t) scope state))

(provide 'elsa-extension-eieio)
