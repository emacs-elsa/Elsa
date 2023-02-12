;; -*- lexical-binding: t -*-
(require 'elsa-test-helpers)

(require 'elsa)
(require 'elsa-log)

(describe "Annotations"

  (describe "elsa-disable-line"

    (it "should disable errors on the line where the annotation is placed"
      (let ((state (elsa-state)))
        (elsa-test-with-analysed-form "|(1+ \"foo\") ;; elsa-disable-line" form
          :state state
          :errors-var errors
          (elsa-log "ignored lines %s" (oref state ignored-lines))
          (expect errors :to-be nil)))))

  (describe "elsa-disable-next-line"

    (it "should disable errors on the line after which the annotation is placed"
      (elsa-test-with-analysed-form ";; elsa-disable-next-line\n|(1+ \"foo\")" form
        :errors-var errors
        (expect errors :to-be nil)))))
