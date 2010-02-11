#lang typed/scheme

(require
 scheme/match
 "types.ss"
 "util.ss")


(: place-ref (Place Integer Integer -> Real))
(define (place-ref p x y)
  (match-define (struct Place (p-x p-y pts)) p)
  (let ([c (place-coords->index p x y)])
    (if c
        (vector-ref pts c)
        (raise-type-error
         'place-ref
         (format "Expected indices in the range (0,0),~a, received: "
                 (list p-x p-y))
         (list x y)))))


(: place-coords->index (Place Integer Integer -> (Option Natural)))
(define (place-coords->index p x y)
  (match-define (struct Place (p-x p-y pts)) p)
  (if (and (exact-nonnegative-integer? x)
           (exact-nonnegative-integer? y)
           (< x p-x) (< y p-y))
      (+ (* y p-x) x)
      #f))
  
(: place-has-point? (Place Integer Integer -> Boolean))
(define (place-has-point? p x y)
  (match-define (struct Place (p-x p-y pts)) p)
  (and (< -1 x p-x) (< -1 y p-y)))
  
(: place-ll (Place Integer Integer -> Real))
(define (place-ll p x y)
  (if (place-has-point? p x y)
      (let ([n (place-ref p x y)])
        (assert (number->real (log (/ n (+ n 1.0))))))
      (assert (number->real (log 0.5)))))


(provide
 place-ref
 place-has-point?
 place-ll)