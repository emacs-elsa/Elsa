(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))
(require 'elsa-check)
(require 'elsa-rules-list)

(defclass elsa-ruleset () () :abstract t)

(cl-defgeneric elsa-ruleset-load (this)
  "Load a ruleset.")

(defclass elsa-ruleset-dead-code (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-dead-code))
  (add-to-list 'elsa-checks (elsa-check-if-useless-condition))
  (add-to-list 'elsa-checks (elsa-check-cond-useless-condition))
  (add-to-list 'elsa-checks (elsa-check-or-unreachable-code))
  (add-to-list 'elsa-checks (elsa-check-unreachable-code))
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

(defclass elsa-ruleset-functions (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-functions))
  (add-to-list 'elsa-checks (elsa-check-useless-type-guard))
  )

(defclass elsa-ruleset-default (elsa-ruleset) ())

(cl-defmethod elsa-ruleset-load ((this elsa-ruleset-default))
  (elsa-ruleset-load (elsa-ruleset-dead-code))
  (elsa-ruleset-load (elsa-ruleset-style))
  (elsa-ruleset-load (elsa-ruleset-error))
  (elsa-ruleset-load (elsa-ruleset-functions))
  (elsa-ruleset-load (elsa-ruleset-variables)))

(provide 'elsa-ruleset)
