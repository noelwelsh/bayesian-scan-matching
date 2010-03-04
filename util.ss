#lang typed/scheme

(require
 scheme/math)


(: number->exact-integer (Number -> (Option Integer)))
(define (number->exact-integer number)
  (cond
   [(exact-integer? number) number]
   [(integer? number)
    (let ([val (inexact->exact number)])
      (if (exact-integer? val)
          val
          #f))]
   [else #f]))

(: number->real (Number -> (Option Real)))
(define (number->real number)
  (if (real? number)
      number
      #f))

(: number->natural (Number -> (Option Natural)))
(define (number->natural number)
  (if (exact-nonnegative-integer? number)
      number
      #f))

(: assert (All (a) ((Option a) -> a)))
(define (assert v)
  (if v
      v
      (raise-type-error 'assert "Assertion failed" v)))

(: degrees->radians (Real -> Real))
(define (degrees->radians deg)
  (/ (* deg (* 2 pi)) 360))


(provide
 number->exact-integer
 number->real
 number->natural
 
 assert

 degrees->radians)
 