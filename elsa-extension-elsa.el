(require 'elsa-analyser)

(defun elsa--analyse:elsa-make-type (form scope state)
  (elsa--analyse-macro form nil scope state))

(provide 'elsa-extension-elsa)
