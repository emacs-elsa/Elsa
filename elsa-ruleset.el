(require 'eieio)
(require 'elsa-check)
(require 'elsa-rules-list)

(defclass elsa-ruleset () () :abstract t)

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset))
  "Load a ruleset.")

(defclass elsa-ruleset-dead-code (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-dead-code))
  (add-to-list 'elsa-checks (elsa-check-if-useless-condition))
  (add-to-list 'elsa-checks (elsa-check-cond-useless-condition))
  (add-to-list 'elsa-checks (elsa-check-or-unreachable-code))
  )

(defclass elsa-ruleset-style (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-style))
  (add-to-list 'elsa-checks (elsa-check-if-useless-then-progn))
  (add-to-list 'elsa-checks (elsa-check-if-useless-else-progn))
  (add-to-list 'elsa-checks (elsa-check-if-to-when))
  (add-to-list 'elsa-checks (elsa-check-lambda-eta-conversion))
  (add-to-list 'elsa-checks (elsa-check-symbol-naming))
  (add-to-list 'elsa-checks (elsa-check-public-functions-have-docstring))
  )

(defclass elsa-ruleset-error (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-error))
  (add-to-list 'elsa-checks (elsa-check-error-message))
  )

(defclass elsa-ruleset-variables (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-variables))
  (add-to-list 'elsa-checks (elsa-check-unbound-variable))
  )

(provide 'elsa-ruleset)
