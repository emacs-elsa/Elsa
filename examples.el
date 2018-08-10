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

(let ((a 1)
      (b a))
  a)

(let* ((c 1)
       (d c))
  a)

;; form is checked according to the definition of use-foo and foo
;; variable
(use-foo foo "")

(use-foo "or a literal value can be provided" nil)

(use-foo 1 1) ;; fails on integer literal

;; (elsa :: (list symbol) -> (cons (nullable string) (or int string)))
;; (elsa :: List Symbol -> Cons String? (Int | String))
;; (elsa :: String & Keyword)
;; (elsa :: Vector Int -> Keyword)
;; (elsa :: String... -> String)
(defun concat (&rest strings))
;; (elsa :: Marker -> a -> b -> a)
