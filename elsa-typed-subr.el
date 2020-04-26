(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'match-string 'elsa-type (elsa-make-type (function (int (or string nil)) (or string nil))))
(put 'last 'elsa-type (elsa-make-type (function ((list mixed) (or int nil)) (cons mixed mixed))))

(provide 'elsa-typed-subr)
