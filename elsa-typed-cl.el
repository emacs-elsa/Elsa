(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'cl-incf 'elsa-type (elsa-make-type Number -> Number? -> Number))
(put 'cl-decf 'elsa-type (elsa-make-type Number -> Number? -> Number))

(provide 'elsa-typed-cl)
