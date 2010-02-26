#lang typed/scheme

(require
 scheme/match
 scheme/vector
 "types.ss"
 "point.ss"
 "grid-scan.ss"
 "util.ss")


(: place-ref (Place Integer Integer -> Real))
(define (place-ref p x y)
  (match-define (struct Place (p-x p-y pts)) p)
  (let ([c (place-coords->index p x y)])
    (if c
        (vector-ref pts c)
        (raise-type-error
         'place-ref
         (format "Expected indices in the range (0,0),~a, received: "
                 (list p-x p-y))
         (list x y)))))


(: place-coords->index (Place Integer Integer -> (Option Natural)))
(define (place-coords->index p x y)
  (match-define (struct Place (p-x p-y pts)) p)
  (if (and (exact-nonnegative-integer? x)
           (exact-nonnegative-integer? y)
           (< x p-x) (< y p-y))
      (+ (* y p-x) x)
      #f))
  
(: place-has-point? (Place Integer Integer -> Boolean))
(define (place-has-point? p x y)
  (match-define (struct Place (p-x p-y pts)) p)
  (and (< -1 x p-x) (< -1 y p-y)))
  
(: place-ll (Place Integer Integer -> Real))
(define (place-ll p x y)
  (if (place-has-point? p x y)
      (let ([n (place-ref p x y)])
        (assert (number->real (log (/ n (+ n 1.0))))))
      (assert (number->real (log 0.5)))))

(: place-copy (Place -> Place))
(define (place-copy p)
  (match-define (struct Place (p-x p-y pts)) p)
  (make-Place p-x p-y (vector-copy pts)))

(: place-expand-to-bb (Place Grid-Point Grid-Point -> (values Place Integer Integer)))
;;
;; Return an expanded place and x and y offsets to convert
;; old coordinate system to new one.
(define (place-expand-to-bb p lt rb)
  (match-define (struct Place (p-x p-y pts)) p)
  (define lt-x (point-x lt))
  (define lt-y (point-y lt))
  (define new-lt-x (if (< lt-x 0) lt-x 0))
  (define new-lt-y (if (< lt-y 0) lt-y 0))
  (define rb-x (point-x rb))
  (define rb-y (point-y rb))
  (define new-rb-x (if (< p-x rb-x) rb-x p-x))
  (define new-rb-y (if (< p-y rb-y) rb-y p-y))

  (define new-p-x
    (assert (number->exact-nonnegative-integer (- new-rb-x new-lt-x))))
  (define new-p-y
    (assert (number->exact-nonnegative-integer (- new-rb-y new-lt-y))))
  (define new-size (* new-p-x new-p-y))
  (define new-pts (make-vector new-size #{1.0 :: Real}))

  (let loop-x ([x 0])
    (unless (= x p-x)
      (let loop-y ([y 0])
        (unless (= y p-y)
          (let* ([pt (vector-ref pts (+ (* y p-x) x))]
               [new-x (- x new-lt-x)]
               [new-y (- y new-lt-y)]
               [new-idx (+ (* new-y new-p-x) new-x)])y
          (vector-set! new-pts new-idx pt))
          (loop-y (add1 y))))
      (loop-x (add1 x))))
  ;;(for* ([x (in-range p-x)]
  ;;       [y (in-range p-y)])
  ;;      (let* ([pt (vector-ref pts (+ (* y p-x) x))]
  ;;             [new-x (- x new-lt-x)]
  ;;             [new-y (- y new-lt-y)]
  ;;             [new-idx (+ (* new-y new-p-x) new-x)])y
  ;;        (vector-set! new-pts new-idx pt)))
  (values (make-Place new-p-x new-p-y new-pts) new-lt-x new-lt-y))
  
(: place-add (Place Grid-Scan -> Place))
;;
;; Update a place with a scan. The scan must be already
;; transformed by an appropriate pose
(define (place-add p s)
  (define-values (lt rb) (grid-scan-bb s))
  (define-values (new-p xt yt) (place-expand-to-bb p lt rb))
  (match-define (struct Place (p-x p-y pts)) new-p)
  (for ([pt (in-vector s)])
       (define x (- (point-x pt) xt))
       (define y (- (point-y pt) yt))
       (define idx (place-coords->index new-p x y))
       (when idx
         (vector-add1! pts idx)))
  new-p)

(: place-remove (Place Grid-Scan -> Place))
;;
;; Remove a scan from a place. The scan must be already
;; transformed by an appropriate pose
(define (place-remove p s)
  (define new-p (place-copy p))
  (match-define (struct Place (p-x p-y pts)) new-p)
  (for ([pt (in-vector s)])
       (define x (point-x pt))
       (define y (point-y pt))
       (define idx (place-coords->index new-p x y))
       (when idx
         (vector-sub1! pts idx)))
  new-p)

;;
;; Utilities
;;
;; (Duplicating some functionality from
;; schematics/numeric:1/vector with types definitions)

(: vector-add1! ((Vectorof Real) Natural -> Void))
(define (vector-add1! v idx)
  (vector-set! v idx (add1 (vector-ref v idx))))

(: vector-sub1! ((Vectorof Real) Natural -> Void))
(define (vector-sub1! v idx)
  (vector-set! v idx (sub1 (vector-ref v idx))))

(provide
 place-ref
 place-has-point?
 place-ll

 place-add
 place-remove)