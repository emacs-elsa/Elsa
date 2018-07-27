(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'not 'elsa-type (elsa-make-type-fn mixed -> bool))
(put 'stringp 'elsa-type (elsa-make-type-fn mixed -> bool))

(put 'string-to-number 'elsa-type (elsa-make-type-fn string -> number))

(provide 'elsa-typed-builtin)
