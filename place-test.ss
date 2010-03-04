#lang scheme/base

(require
 (planet schematics/schemeunit:3/test)
 "place.ss"
 "point.ss"
 "types.ss"
 "scan-match.ss"
 "grid-scan.ss"
 "test-data.ss")

(define/provide-test-suite place-tests
  (test-case
   "place-ref"
   (check = (place-ref example-place 0 0) 1)
   (check = (place-ref example-place 1 1) 1)
   (check = (place-ref example-place 2 2) 2)
   (check = (place-ref example-place 3 3) 2)
   (check = (place-ref example-place 4 4) 1)
   (check-exn exn:fail? (lambda () (place-ref example-place 0 5)))
   (check-exn exn:fail? (lambda () (place-ref example-place 11 0))))

  (test-case
   "place-ref w/ first element not at 0,0"
   (check = (place-ref offset-place -4 -4) 1)
   (check = (place-ref offset-place -3 -3) 1)
   (check = (place-ref offset-place -2 -2) 2)
   (check = (place-ref offset-place -1 -1) 2)
   (check = (place-ref offset-place 0 0) 1)
   (check = (place-ref offset-place 1 0) 2)
   (check-exn exn:fail? (lambda () (place-ref offset-place 0 -5)))
   (check-exn exn:fail? (lambda () (place-ref offset-place 11 0))))
  
  (test-case
   "place-add and place-remove are inverses"
   (define sample (scan-match/best example-place example-grid-scan))
   (define pose (sample-pose sample))
   (define scan (grid-scan-transform/pose example-grid-scan pose))
   (check-equal?
    (for/fold ([place example-place])
        ([i (in-range 10)])
      (if (odd? i)
          (place-remove place scan)
          (place-add place scan)))
    example-place))

  (test-case
   "place-add updates the correct locations"
   (define posterior (place-add example-place example-grid-scan))
   (for ([pt (in-vector example-grid-scan)])
        (define x (vector-ref pt 0))
        (define y (vector-ref pt 1))
        (if (place-has-point? example-place x y)
            (check-equal?
             (place-ref posterior x y)
             (add1 (place-ref example-place x y)))
            (fail "Place does not expand to contain added points."))))

  (test-case
   "place expands to contain scan on update"
   (define posterior (place-add example-place translated-grid-scan))
   (check-equal? (Place-w posterior) 14)
   (check-equal? (Place-h posterior) 7)
   (check-equal? (Place-x posterior) -4)
   (check-equal? (Place-y posterior) -2)
   (check = (place-ref posterior -4 -2) 2)
   (check = (place-ref posterior -3 -2) 2)
   (check = (place-ref posterior 0 0) 1)
   (check = (place-ref posterior 3 0) 2))
  )