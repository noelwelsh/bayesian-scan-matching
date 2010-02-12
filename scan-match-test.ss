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
  )