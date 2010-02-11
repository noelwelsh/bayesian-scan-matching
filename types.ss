#lang typed-scheme

(define-type-alias Point (Vectorof Number))
(define-type-alias Scan (Vectorof Point))

(define-type-alias Grid-Point (Vectorof Integer))
(define-type-alias Grid-Scan (Vectorof Grid-Point))

(provide
 (all-defined-out))