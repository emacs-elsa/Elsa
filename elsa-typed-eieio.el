(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'eieio-object-class 'elsa-type (elsa-make-type (function (mixed) symbol)))

(provide 'elsa-typed-eieio)
