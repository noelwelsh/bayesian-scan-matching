#lang typed/scheme

(require
 scheme/unsafe/ops
 "base.ss"
 "types.ss"
 "point.ss"
 "grid-scan.ss"
 "place.ss"
 "pose.ss"
 "util.ss")

(: log-likelihood (Place Grid-Scan -> (Option Real)))
;; Computes the log-likelihood for the scan coming from the
;; place. We assume coordinates have already been adjusted
;; etc. Returns #f if the scan is completely outside the
;; place. Otherwise a log-likelihood is always returned.
(define (log-likelihood place grid-scan)
  (: loop (Real Boolean Natural -> (values Real Boolean)))
  (define (loop ll inside? idx) 
      (if (= idx (unsafe-vector-length grid-scan))
          (values ll inside?)
          (let* ([pt (unsafe-vector-ref grid-scan idx)]
                 [x (point-x pt)]
                 [y (point-y pt)])
            (if (place-has-point? place x y)
                (loop (unsafe-fl+ (place-ll place x y) ll) #t (add1 idx))
                (loop (unsafe-fl+ (place-ll place x y) ll) inside? (add1 idx))))))
  (: ll Real) (: inside? Boolean)
  (define-values (ll inside?)
    (loop 0.0 #f 0))

    ;; (for/fold ([#{ll : Real} 0.0]
    ;;            [#{inside? : Boolean} #f])
    ;;     ([pt (in-vector grid-scan)])
    ;;   (define: x : Integer (point-x pt))
    ;;   (define: y : Integer (point-y pt))
    ;;   (if (place-has-point? place x y)
    ;;       (values (+ (place-ll place x y) ll) #t)
    ;;       (values (+ (place-ll place x y) ll) inside?)))
  (if inside? ll #f))

(: scan-match
   (Place Grid-Scan Natural -> (Listof Sample))
   ;(case-lambda 
   ;  (Place Grid-Scan -> (Listof Sample))
   ;  (Place Grid-Scan Natural -> (Listof Sample))))
   )
(define (scan-match place grid-scan angle-increment)
  (define: (add-unit [v : Real]) : Real (unsafe-fl+ v 1))
  (define: (sub-unit [v : Real]) : Real (unsafe-fl- v 1))
  (: transformed-ll (Real Real Real -> (Option Real)))
  (define (transformed-ll x y a)
    (log-likelihood
     place
     (grid-scan-transform grid-scan x y a)))
  (: sample-y-axis
     ((Listof Sample) Real Real (Real -> Real) Real -> (Listof Sample)))
  ;; Given a fixed x and angle sample the y-axis in the
  ;; direction given by y-inc
  (define (sample-y-axis samples x y y-inc angle)
    (let ([ll (transformed-ll x y angle)])
      (if ll
          (sample-y-axis
           (cons (cons ll (vector x y angle)) samples)
           x
           (y-inc y) y-inc
           angle)
          samples)))
  (: sample-x-axis ((Listof Sample) Real (Real -> Real) Real -> (Listof Sample)))
  ;; Sample the half-plane in the direction given by x-inc
  (define (sample-x-axis samples x x-inc angle)
    (let ([ll (transformed-ll x 0 angle)])
      (if ll
          (sample-x-axis
           (sample-y-axis
            (sample-y-axis (cons (cons ll (vector x 0 angle)) samples)
                           x
                           (add-unit 0) add-unit
                           angle)
            x
            (sub-unit 0) sub-unit
            angle)
           (x-inc x) x-inc angle)
          samples)))
  
  (for/fold ([samples null])
      ([angle (in-range 0 360 angle-increment)])
    (sample-x-axis
     (sample-x-axis samples 0 add-unit (degrees->radians angle))
     (sub-unit 0) sub-unit (degrees->radians angle))))

(: scan-match/best (Place Grid-Scan -> Sample))
;; Returns the best sample
(define (scan-match/best place grid-scan)
  ;; Slow implementation
  (car
   (sort (scan-match place grid-scan 1)
         (lambda: ([s1 : Sample] [s2 : Sample])
           (let ([ll1 (car s1)]
                 [ll2 (car s2)]
                 [tx1 (cdr s1)]
                 [tx2 (cdr s2)])
             (cond
              [(> ll1 ll2) #t]
              [(= ll1 ll2) (pose< tx1 tx2)]
              [else #f]))))))
               

(provide
 log-likelihood
 scan-match
 scan-match/best)