#lang typed/scheme

(: likelihood (Prior Grid-Scan -> (Option Real)))
;; Computes the log-likelihood for the scan coming from the
;; prior. We assume coordinates have already been adjusted
;; etc. Returns #f if the scan is completely outside the
;; prior. Otherwise a log-likelihood is always returned.
(define (log-likelihood prior grid-scan)
  (define-values (ll inside?)
    (for/fold ([ll 0]
               [inside? #f])
        ([pt (in-vector grid-scan)])
      (define x (point-x pt))
      (define y (point-y pt))
      (if (prior-has-point? prior x y)
          (values (+ (prior-ll prior x y) ll) #t)
          (values (+ (prior-ll prior x y) ll) inside?))))
  (if inside? ll #f))


(provide
 log-likelihood)