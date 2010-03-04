#lang typed/scheme

(require
 "types.ss")

(define example-place
  (make-Place 0 0 10 5
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

;; Scan that is half outside example-place
;;
;; exact-match-grid-scan translated -6,-2
(define translated-grid-scan
  (vector (vector -4 -2) (vector -3 -2) (vector -2 -2)
          (vector -4 -1)
          (vector -4 0)
          (vector -4 1) (vector -3 1) (vector -2 1)))

;; Place and scan small enough that we can work all the
;; possible scan matches
(define small-place
  (make-Place 0 0 2 2 #(2 1 1 2)))

(define small-grid-scan
  (vector (vector 0 0) (vector 0 1)))

;; Place where the first element is not at 0,0
(define offset-place
  (make-Place -4 -4 10 5
              #(1 1 2 2 2 2 2 2 1 1
                1 1 2 1 1 1 1 1 2 1
                1 1 2 1 1 1 1 2 1 1
                1 1 2 2 2 1 1 2 1 1
                1 1 1 1 1 2 2 1 1 1)))


(provide
 (all-defined-out))