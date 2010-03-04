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
  (let ([idx (place-coords->index p x y)])
    (if idx
        (vector-ref (Place-points p) idx)
        (match-let ([(struct Place (p-x p-y p-w p-h pts)) p])
          (raise-type-error
           'place-ref
           (format "Indices in the range ~a to ~a"
                   (list p-x p-y)
                   (list (sub1 (+ p-x p-w)) (sub1 (+ p-x p-h))))
           (list x y))))))


(: place-coords->index (Place Integer Integer -> (Option Natural)))
(define (place-coords->index p x y)
  (match-define (struct Place (p-x p-y p-w p-h pts)) p)
  (if (and (<= p-x x) (<= p-y y)
           (< x (+ p-x p-w)) (< y (+ p-y p-h)))
      (number->exact-nonnegative-integer (coords->index x y p-x p-y p-w))
      #f))
  
(: place-has-point? (Place Integer Integer -> Boolean))
(define (place-has-point? p x y)
  (if (place-coords->index p x y) #t #f))
  
(: place-ll (Place Integer Integer -> Real))
(define (place-ll p x y)
  (let ([idx (place-coords->index p x y)])
    (if idx
        (let ([n (vector-ref (Place-points p) idx)])
          (assert (number->real (log (/ n (+ n 1.0))))))
        (assert (number->real (log 0.5))))))

(: place-copy (Place -> Place))
(define (place-copy p)
  (match-define (struct Place (p-x p-y p-w p-h pts)) p)
  (make-Place p-x p-y p-w p-h (vector-copy pts)))

(: place-expand-to-bb (Place Grid-Point Grid-Point -> (values Place Integer Integer)))
;;
;; Return an expanded place and x and y offsets to convert
;; old coordinate system to new one.
(define (place-expand-to-bb p lt rb)
  (match-define (struct Place (p-x p-y p-w p-h pts)) p)
  (define lt-x (point-x lt))
  (define lt-y (point-y lt))
  (define new-lt-x (if (< lt-x p-x) lt-x p-x))
  (define new-lt-y (if (< lt-y p-y) lt-y p-y))
  (define rb-x (point-x rb))
  (define rb-y (point-y rb))
  (define new-rb-x (if (< (+ p-x p-w) rb-x) rb-x (+ p-x p-w)))
  (define new-rb-y (if (< (+ p-y p-h) rb-y) rb-y (+ p-y p-h)))

  (define new-p-w
    (assert (number->exact-nonnegative-integer (- new-rb-x new-lt-x))))
  (define new-p-h
    (assert (number->exact-nonnegative-integer (- new-rb-y new-lt-y))))
  (define new-size (* new-p-w new-p-h))
  (define new-pts (make-vector new-size #{1.0 :: Real}))

  ;; Copy pts to new-pts
  (let loop-x ([x p-x])
    (unless (= x (+ p-x p-w))
      (let loop-y ([y p-y])
        (unless (= y (+ p-y p-h))
          (let* ([pt (vector-ref pts (coords->index x y p-x p-y p-w))]
                 [new-idx (coords->index x y new-lt-x new-lt-y new-p-w)])
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
  (values (make-Place new-lt-x new-lt-y new-p-w new-p-h new-pts)
          new-lt-x new-lt-y))
  
(: place-add (Place Grid-Scan -> Place))
;;
;; Update a place with a scan. The scan must be already
;; transformed by an appropriate pose
(define (place-add p s)
  (define-values (lt rb) (grid-scan-bb s))
  (define-values (new-p xt yt) (place-expand-to-bb p lt rb))
  (match-define (struct Place (p-x p-y p-w p-h pts)) new-p)
  (for ([pt (in-vector s)])
       (define x (point-x pt))
       (define y (point-y pt))
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
  (match-define (struct Place (p-x p-y p-w p-h pts)) new-p)
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

(: coords->index (Integer Integer Integer Integer Natural -> Integer))
(define (coords->index x y p-x p-y p-w)
  (+ (* (- y p-y) p-w) (- x p-x)))

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