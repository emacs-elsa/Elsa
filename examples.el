;; An example file for analysis.  This file is *NOT* loaded, we only
;; read it and analyse statically.

(require 'dash)
(require 'elsa-types)

;; elsa-type annotates any form with this type
(elsa-cast string
  (defvar foo 'lala))

(defun use-foo (foo bar)
  "Return string representation of FOO."
  (declare (elsa-args string? string) ;; nullable string
           (elsa-return string))
  (symbol-name foo))

;; form is checked according to the definition of use-foo and foo
;; variable
(use-foo foo "")

(use-foo "or a literal value can be provided" nil)

(use-foo 1 1) ;; fails on integer literal
