#lang typed-scheme

(: point-x (All (a) ((Vectorof a) -> a)))
(define (point-x pt)
  (vector-ref pt 0))

(: point-y (All (a) ((Vectorof a) -> a)))
(define (point-y pt)
  (vector-ref pt 1))


(provide
 point-x
 point-y)