(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'syntax-ppss 'elsa-type (elsa-make-type (function ((or number nil)) (list mixed))))

(provide 'elsa-typed-syntax)
