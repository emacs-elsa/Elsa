(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'match-string 'elsa-type (elsa-make-type Int -> String? -> String?))

(provide 'elsa-typed-subr)
