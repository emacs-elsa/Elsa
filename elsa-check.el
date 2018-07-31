(defvar elsa-checks nil)

(defun elsa-check-add (check)
  (let ((name (oref check name)))
    (unless (--any? (eq (oref it name) name) elsa-checks)
      (push check elsa-checks))))

(defclass elsa-check () () :abstract t)

(cl-defgeneric elsa-check-should-run ((this elsa-check) form scope) nil)

(cl-defgeneric elsa-check-check ((this elsa-check) form scope) nil)

(provide 'elsa-check)
