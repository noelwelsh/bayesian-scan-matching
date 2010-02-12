#lang typed/scheme

(require
 "types.ss")

(define example-place
  (make-Place 10 5
              #(1 1 2 2 2 2 2 2 1 1
                1 1 2 1 1 1 1 1 2 1
                1 1 2 1 1 1 1 2 1 1
                1 1 2 2 2 1 1 2 1 1
                1 1 1 1 1 2 2 1 1 1)))

(define example-grid-scan
   (vector (vector 2 0) (vector 3 0) (vector 4 1) (vector 8 1)))


(provide
 (all-defined-out))