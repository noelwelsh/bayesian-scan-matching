#lang scheme/base

(require
 (planet schematics/schemeunit:3/test)
 (planet cce/fasttest:3:6/random)
 "grid.ss")

(define (random-point)
  (vector->immutable-vector (vector (random-real) (random-real))))

(define (random-scan)
  (vector->immutable-vector
   ;; Keep the vector lengths reasonable or this can take a very long time
   (build-vector (random-natural/geometric 1/10 1)
                 (lambda (x) (random-point)))))

(define-check (check-grid-point pt grid-pt unit)
  (define x0 (vector-ref pt 0))
  (define x1 (* unit (vector-ref grid-pt 0)))
  (define y0 (vector-ref pt 1))
  (define y1 (* unit (vector-ref grid-pt 1)))
  
  (if (and (<= x1 x0 (+ x1 unit))
           (<= y1 y0 (+ y1 unit)))
      #t
      (fail (format "Points ~a,~a not with ~a" pt grid-pt unit))))


(define/provide-test-suite grid-tests
  (test-case
   "discretise"
   (define vals
     (build-list 1000 (lambda (i) (random-positive-real))))
   (define units
     (build-list 1000 (lambda (i) (/ (random-positive-real) 10))))
   (map
    (lambda (value unit)
      (define result (discretise value unit))
      (check <= (* result unit) value)
      (check <= value (+ (* result unit) unit)))
    vals
    units))

  (test-case
   "scan->grid-scan"
   (map
    (lambda (scan)
      (define grid-scan (scan->grid-scan scan 0.1))
      (check = (vector-length scan) (vector-length grid-scan))
      (for ([pt (in-vector scan)]
            [grid-pt (in-vector grid-scan)])
        (check-grid-point pt grid-pt 0.1)))
    (build-list 100 (lambda (_) (random-scan)))))
  )