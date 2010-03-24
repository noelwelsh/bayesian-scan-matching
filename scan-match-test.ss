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
   "log-likelihood for scan with minimal overlap"
   (check =
          (log-likelihood example-place
                          (vector (vector -1 -1) (vector 0 0) (vector -3 -3)))
          (* 3 (log 1/2))))

  (test-case
   "scan-match"
   (define sample (scan-match small-place small-grid-scan 45))
   (display (length sample))(newline)
   ;(display sample)
   (fail "Not implemented"))

  (test-case
   "scan-match/best"
   (define sample (scan-match/best example-place exact-match-grid-scan))
   (check-= (car sample) (* 8 (log 2/3)) 0.00001)
   (check-equal? (cdr sample) (vector 0. 0. 0.)))
  )