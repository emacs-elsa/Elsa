(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'bounds-of-thing-at-point 'elsa-type (elsa-make-type Symbol -> Nil | (Cons Int Int)))

(provide 'elsa-typed-thingatpt)
