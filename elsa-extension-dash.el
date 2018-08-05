(require 'elsa-analyser)

(defun elsa-dash--update-anaphora-scope (scope)
  "Add the `it' variable to SCOPE."
  (let ((it-var (elsa-variable
                 :name 'it
                  ;; TODO: derive type based on the list argument type
                 :type (elsa-make-type 'mixed))))
    (elsa-scope-add-variable scope it-var)
    it-var))

(defun elsa-dash--restore-anaphora-scope (it-var scope)
  "Remove the IT-VAR variable representing `it' from SCOPE."
  (elsa-scope-remove-variable scope it-var))

(defun elsa-dash--analyse-anaphora (form scope state)
  (let ((it-var (elsa-dash--update-anaphora-scope scope)))
    (elsa--analyse-function-call form scope state)
    (elsa-dash--restore-anaphora-scope it-var scope)))

(defun elsa--analyse---first (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse---map (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse---filter (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(provide 'elsa-extension-dash)
