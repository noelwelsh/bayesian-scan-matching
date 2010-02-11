#lang scheme/base

(require
 (planet schematics/schemeunit:3/test)
 "prior.ss"
 "types.ss")

(define example-prior
  (make-prior 10 5
              #(1 1 2 2 2 2 2 2 1 1
                1 1 2 1 1 1 1 1 2 1
                1 1 2 1 1 1 1 2 1 1
                1 1 2 2 2 1 1 2 1 1
                1 1 1 1 1 2 2 1 1 1)))

(define/provide-test-suite prior-tests
  (test-case
   "prior-ref"
   (check = (prior-ref example-prior 0 0) 1)
   (check = (prior-ref example-prior 1 1) 1)
   (check = (prior-ref example-prior 2 2) 2)
   (check = (prior-ref example-prior 3 3) 2)
   (check = (prior-ref example-prior 4 4) 1)
   (check-exn exn:fail? (lambda () (prior-ref example-prior 5 0))))
    
  )