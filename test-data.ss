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

;; Exactly matches the left hand corner of example-place
;; without any rotation or translation
(define exact-match-grid-scan
  (vector (vector 2 0) (vector 3 0) (vector 4 0)
          (vector 2 1)
          (vector 2 2)
          (vector 2 3) (vector 3 3) (vector 4 3)))

(provide
 (all-defined-out))