;; (elsa-checks :: [Mixed])
(defvar elsa-checks nil)

(defun elsa-check-add (check)
  (let ((name (oref check name)))
    (unless (--any? (eq (oref it name) name) elsa-checks)
      (push check elsa-checks))))

(defclass elsa-check () () :abstract t
  :documentation
  "A check is essentially a callback run at every node of the AST.

A check has to implement two methods:

- `elsa-check-should-run' to decide if the check should process
  the current node.
- `elsa-check-check' which performs the actual computation.")

(cl-defgeneric elsa-check-should-run ((this elsa-check) form scope state)
  "Decide if the check should run on FORM."
  nil)

(cl-defgeneric elsa-check-check ((this elsa-check) form scope state)
  "Run THIS check on FORM in SCOPE."
  nil)

(provide 'elsa-check)
