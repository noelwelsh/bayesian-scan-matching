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
   "place expands left/up to contain scan on update"
   (define posterior (place-add example-place translated-grid-scan))
   (check-equal? (Place-w posterior) 14)
   (check-equal? (Place-h posterior) 7)
   (check-equal? (Place-x posterior) -4)
   (check-equal? (Place-y posterior) -2)
   (for ([pt (in-vector translated-grid-scan)])
        (check = (place-ref posterior
                            (vector-ref pt 0) (vector-ref pt 1)) 2))
   (check = (place-ref posterior 0 0) 1)
   (check = (place-ref posterior 3 0) 2)
   (check = (place-ref posterior 9 4) 1))

  (test-case
   "place expands right/down to contain scan on update"
   (define posterior (place-add small-place exact-match-grid-scan))
   (check-equal? (Place-w posterior) 5)
   (check-equal? (Place-h posterior) 4)
   (check-equal? (Place-x posterior) 0)
   (check-equal? (Place-y posterior) 0)
   (for ([pt (in-vector exact-match-grid-scan)])
        (check = (place-ref posterior
                            (vector-ref pt 0) (vector-ref pt 1)) 2))
   (check = (place-ref posterior 0 0) 2)
   (check = (place-ref posterior 1 0) 1)
   (check = (place-ref posterior 0 1) 1)
   (check = (place-ref posterior 1 1) 2))
  
  (test-case
   "grid-scan->place"
   (define p (grid-scan->place translated-grid-scan))
   (check-equal? (Place-w p) 3)
   (check-equal? (Place-h p) 4)
   (check-equal? (Place-x p) -4)
   (check-equal? (Place-y p) -2)
   (for ([pt (in-vector translated-grid-scan)])
        (check = (place-ref p (vector-ref pt 0) (vector-ref pt 1)) 2))
   (check = (place-ref p -2 0) 1))

  )