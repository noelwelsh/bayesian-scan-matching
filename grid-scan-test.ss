#lang scheme/base

(require
 scheme/math
 (planet schematics/schemeunit:3/test)
 "grid-scan.ss"
 "test-data.ss")

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

  (test-case
   "grid-scan-bb"
   (define-values (lt1 rb1) (grid-scan-bb example-grid-scan))
   (define-values (lt2 rb2) (grid-scan-bb translated-grid-scan))
   (check-equal? lt1 (vector 2 0))
   (check-equal? rb1 (vector 4 3))
   (check-equal? lt2 (vector -4 -2))
   (check-equal? rb2 (vector -2 1))) 
  )