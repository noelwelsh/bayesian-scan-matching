#lang typed-scheme

;; Utilities for type injections

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


;; Utilities to deal with the annoying Option type

(define-syntax try
  (syntax-rules ()
    [(try expr)
     (let ([val expr])
       (if val val #f))]
    [(try expr0 expr1 ...)
     (let ([val expr0])
       (if val
           (try expr1 ...)
           #f))]))

(define-syntax try/fail
  (syntax-rules ()
    [(try/fail expr)
     (let ([val expr])
       (if val
           val
           (raise-type-error
            'try/fail
            (format "non-#f result of expression ~a" (quote expr))
            val)))]
    [(try/fail expr0 expr1 ...)
     (let ([val expr0])
       (if val
           (try/fail expr1 ...)
           (raise-type-error
            'try/fail
            (format "non-#f result of expression ~a" (quote expr0))
            val)))]))


(provide
 number->exact-integer
 
 try
 try/fail)