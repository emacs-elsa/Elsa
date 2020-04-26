(require 'elsa-types)
(require 'elsa-type-helpers)

(put 'bounds-of-thing-at-point 'elsa-type (elsa-make-type (function (symbol) (or nil (cons int int)))))

(put 'symbol-at-point 'elsa-type (elsa-make-type (or symbol nil)))

(provide 'elsa-typed-thingatpt)
