#lang typed-scheme

(require
 scheme/match
 "types.ss")


(: prior-ref (Prior Integer Integer -> Number))
(define (prior-ref p x y)
  (match-define (struct Prior (p-x p-y pts)) p)
  (if (prior-has-point? p x y)
      (vector-ref pts (+ (* y p-x) p-x))
      (raise-type-error
       'prior-ref
       (format "Expected indices in the range ~a, received: "
               (list p-x p-y))
       (list x y))))

(: prior-has-point? (Prior Integer Integer -> Boolean))
(define (prior-has-point? p x y)
  (match-define (struct Prior (p-x p-y pts)) p)
  (and (< -1 x p-x) (< -1 y p-y)))
  
(: prior-ll (Prior Integer Integer -> Number))
(define (prior-ll p x y)
  (if (prior-has-point? p x y)
      (let ([n (prior-ref p x y)])
        (log (/ n (+ n 1.0))))
      (log 0.5)))


(provide
 prior-ref
 prior-has-point?
 prior-ll)