#lang typed/scheme

(require
 scheme/match
 "types.ss"
 "util.ss")


(: prior-ref (Prior Integer Integer -> Real))
(define (prior-ref p x y)
  (match-define (struct Prior (p-x p-y pts)) p)
  (let ([c (prior-coords->index p x y)])
    (if c
        (vector-ref pts c)
        (raise-type-error
         'prior-ref
         (format "Expected indices in the range (0,0),~a, received: "
                 (list p-x p-y))
         (list x y)))))


(: prior-coords->index (Prior Integer Integer -> (Option Natural)))
(define (prior-coords->index p x y)
  (match-define (struct Prior (p-x p-y pts)) p)
  (if (and (exact-nonnegative-integer? x)
           (exact-nonnegative-integer? y)
           (< x p-x) (< y p-y))
      (+ (* y p-x) x)
      #f))
  
(: prior-has-point? (Prior Integer Integer -> Boolean))
(define (prior-has-point? p x y)
  (match-define (struct Prior (p-x p-y pts)) p)
  (and (< -1 x p-x) (< -1 y p-y)))
  
(: prior-ll (Prior Integer Integer -> Real))
(define (prior-ll p x y)
  (if (prior-has-point? p x y)
      (let ([n (prior-ref p x y)])
        (assert (number->real (log (/ n (+ n 1.0))))))
      (assert (number->real (log 0.5)))))


(provide
 prior-ref
 prior-has-point?
 prior-ll)