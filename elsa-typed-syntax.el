(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'syntax-ppss 'elsa-type (elsa-make-type Number? -> List))

(provide 'elsa-typed-syntax)
