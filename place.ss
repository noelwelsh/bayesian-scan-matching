#lang typed/scheme

(require
 scheme/match
 scheme/vector
 "types.ss"
 "point.ss"
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

(: place-add (Place Grid-Scan -> Place))
;;
;; Update a place with a scan. The scan must be already
;; transformed by an appropriate pose
(define (place-add p s)
  (define new-p (place-copy p))
  (match-define (struct Place (p-x p-y pts)) new-p)
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