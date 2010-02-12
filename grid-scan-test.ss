#lang scheme/base

(require
 scheme/math
 (planet schematics/schemeunit:3/test)
 "grid-scan.ss")

(define/provide-test-suite grid-scan-tests
  (test-case
   "grid-scan-transform"
   (check-equal?
    (grid-scan-transform (vector (vector 0 0)) 1 1 0)
    (vector (vector 1 1)))
   (check-equal?
    (grid-scan-transform (vector (vector 1 1)) 2 1 pi)
    ;; Due to numerical inaccuracies this will not be the
    ;; #(1 0) point we'd expect with exact arithmetic.
    (vector (vector 0 0))))
  )