#lang scheme/base

(require
 (planet schematics/schemeunit:3/test)
 "scan-match.ss"
 "types.ss"
 "test-data.ss")

(define/provide-test-suite scan-match-tests
  (test-case
   "log-likelihood for example scan"
   (check =
          (log-likelihood example-place example-grid-scan)
          (+ (log 2/3) (log 2/3) (log 1/2) (log 2/3))))

  (test-case
   "scan-match"
   (display (length (scan-match example-place example-grid-scan)))(newline)
   (fail "Not implemented"))

  (test-case
   "scan-match/best"
   (define sample (scan-match/best example-place example-grid-scan))
   (check = (car sample) (* 8 (log 2/3)))
   (check-equal? (cdr sample) (vector 0 0 0)))
  )