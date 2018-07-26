(defvar elsa-checks nil)

(defun elsa-check-add (check)
  (let ((name (oref check name)))
    (unless (--any? (eq (oref it name) name) elsa-checks)
      (push check elsa-checks))))

(defclass elsa-check ()
  ((name :type symbol :initarg :name)
   (predicate :type function :initarg :predicate)
   (check :type function :initarg :check)))

(cl-defmethod elsa-check-should-run ((this elsa-check) form)
  (funcall (oref this predicate) form))

(cl-defmethod elsa-check-check ((this elsa-check) form)
  (funcall (oref this check) form))

(provide 'elsa-check)
