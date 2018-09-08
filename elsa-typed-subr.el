(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'match-string 'elsa-type (elsa-make-type Int -> String? -> String?))
(put 'last 'elsa-type (elsa-make-type [Mixed] -> Int? -> Cons))

(provide 'elsa-typed-subr)
