(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'eieio-object-class 'elsa-type (elsa-make-type Mixed -> Symbol))

(provide 'elsa-typed-eieio)
