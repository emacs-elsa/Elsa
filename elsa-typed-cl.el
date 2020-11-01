(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'cl-incf 'elsa-type (elsa-make-type (function (number (or number nil)) number)))
(put 'cl-decf 'elsa-type (elsa-make-type (function (number (or number nil)) number)))

(provide 'elsa-typed-cl)
