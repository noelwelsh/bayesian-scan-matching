#lang scheme/base

(require
 (planet schematics/schemeunit:3/test)
 "place.ss"
 "types.ss")

(define example-place
  (make-Place 10 5
              #(1 1 2 2 2 2 2 2 1 1
                1 1 2 1 1 1 1 1 2 1
                1 1 2 1 1 1 1 2 1 1
                1 1 2 2 2 1 1 2 1 1
                1 1 1 1 1 2 2 1 1 1)))

(define/provide-test-suite place-tests
  (test-case
   "place-ref"
   (check = (place-ref example-place 0 0) 1)
   (check = (place-ref example-place 1 1) 1)
   (check = (place-ref example-place 2 2) 2)
   (check = (place-ref example-place 3 3) 2)
   (check = (place-ref example-place 4 4) 1)
   (check-exn exn:fail? (lambda () (place-ref example-place 0 5)))
   (check-exn exn:fail? (lambda () (place-ref example-place 11 0))))
    
  )