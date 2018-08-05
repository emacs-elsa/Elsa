(require 'elsa-analyser)

(defun elsa--analyse:oref (form scope state)
  (elsa--analyse-macro form (list t nil) scope state))

(provide 'elsa-extension-eieio)
