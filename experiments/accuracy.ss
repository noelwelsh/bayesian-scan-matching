#lang scheme/base

;; We compare the accuracy of the best match found by the
;; Bayesian scan matching against that found by IDC.

;; The purpose of this experiment is to show:
;;
;; 1. BSM finds a good match. If this isn't the case the
;; posterior will suck
;;
;; 2. The expensive exhaustive search we use has a
;; substantial advantage over faster hill-climbing methods

(require
 (planet williams/science/random-distributions/gaussian)
 (planet cce/fasttest:3/random)
 (planet schematics/numeric:1/vector)
 (planet schematics/icp:1/point)
 "../../freiburg-079/place-1.ss"
 "../base.ss"
 "../types.ss"
 "../grid.ss"
 "../grid-scan.ss"
 "../place.ss"
 "../scan-match.ss")

;; -> (values Real Real Integer)
;;
;; Returns a random x, y, and angle.
(define (random-transform)
  (values
   (random-gaussian 0 10)
   (random-gaussian 0 10)
   (random 360)))

;; Scan -> Real
;;
;; Compute sum of squared error in pose of best match to scan
(define (scan-match-sse scan xt yt a)
  (define grid-scan (scan->grid-scan scan unit))
  (define place (grid-scan->place grid-scan))
  (define sample
    (scan-match/best place (grid-scan-transform grid-scan xt yt a)))
  (define pose (sample-pose sample))
  (vector-sum (vector-map square (vector- pose (vector xt yt a)))))

(define (go!)
  (for/list ([scan (in-vector points)])
    (define-values (xt yt a) (random-transform))
    '(cons
     (scan-match-sse (scan-convert scan) xt yt a)
     (idc-sse scan xt yt a))
    (scan-match-sse (scan-convert scan) xt yt a)))

;; Utils

(define (square x) (* x x))

;; (Vectorof Polar) -> (Vectorof Point)
(define (scan-convert scan)
  (for/vector ([_ (vector-length scan)]
               [pt (in-vector scan)])
    (let ([p (polar->cartesian pt)])
      (vector (cartesian-x p) (cartesian-y p)))))

(provide
 (all-defined-out))