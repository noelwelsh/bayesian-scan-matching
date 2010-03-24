#lang typed/scheme

(require
 scheme/match
 scheme/vector
 scheme/flonum
 scheme/unsafe/ops
 "types.ss"
 "point.ss"
 "grid-scan.ss"
 "util.ss")


(: place-ref (Place Integer Integer -> Real))
(define (place-ref p x y)
  (let ([idx (place-coords->index p x y)])
    (if idx
        (unsafe-flvector-ref (Place-points p) idx)
        (match-let ([(struct Place (p-x p-y p-w p-h pts)) p])
          (raise-type-error
           'place-ref
           (format "Indices in the range ~a to ~a"
                   (list p-x p-y)
                   (list (sub1 (+ p-x p-w)) (sub1 (+ p-y p-h))))
           (list x y))))))


(: place-coords->index (Place Integer Integer -> (Option Natural)))
(define (place-coords->index p x y)
  (match-define (struct Place (p-x p-y p-w p-h pts)) p)
  (if (and (<= p-x x) (<= p-y y)
           (< x (+ p-x p-w)) (< y (+ p-y p-h)))
      (number->natural (coords->index x y p-x p-y p-w))
      #f))
  
(: place-has-point? (Place Integer Integer -> Boolean))
(define (place-has-point? p x y)
  (if (place-coords->index p x y) #t #f))
  
(: place-ll (Place Integer Integer -> Float))
(define (place-ll p x y)
  (let ([idx (place-coords->index p x y)])
    (if idx
        (let ([n (unsafe-flvector-ref (Place-points p) idx)])
          (unsafe-fllog (unsafe-fl/ n (unsafe-fl+ n 1.0))))
        (unsafe-fllog 0.5))))

(: place-copy (Place -> Place))
(define (place-copy p)
  (match-define (struct Place (p-x p-y p-w p-h pts)) p)
  (make-Place p-x p-y p-w p-h (flvector-copy pts)))

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
  (define rb-x (add1 (point-x rb)))
  (define rb-y (add1 (point-y rb)))
  (define new-rb-x (if (< (+ p-x p-w) rb-x) rb-x (+ p-x p-w)))
  (define new-rb-y (if (< (+ p-y p-h) rb-y) rb-y (+ p-y p-h)))

  (define new-p-w
    (assert (number->natural (- new-rb-x new-lt-x))))
  (define new-p-h
    (assert (number->natural (- new-rb-y new-lt-y))))
  (define new-size (* new-p-w new-p-h))
  (define new-pts (make-flvector new-size 1.0))

  ;; Copy pts to new-pts
  (let loop-x ([x p-x])
    (unless (= x (+ p-x p-w))
      (let loop-y ([y p-y])
        (unless (= y (+ p-y p-h))
          (let* ([pt (unsafe-flvector-ref
                      pts
                      (assert (number->natural (coords->index x y p-x p-y p-w))))]
                 [new-idx (assert
                           (number->natural (coords->index x y new-lt-x new-lt-y new-p-w)))])
          (unsafe-flvector-set! new-pts new-idx pt))
          (loop-y (add1 y))))
      (loop-x (add1 x))))

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
         (flvector-add1! pts idx)))
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
         (flvector-sub1! pts idx)))
  new-p)

(: grid-scan->place (Grid-Scan -> Place))
(define (grid-scan->place scan)
  (define-values (lt rb) (grid-scan-bb scan))
  (define lt-x (point-x lt))
  (define lt-y (point-y lt))
  (define rb-x (point-x rb))
  (define rb-y (point-y rb))

  (define w (assert (number->natural (add1 (- rb-x lt-x)))))
  (define h (assert (number->natural (add1 (- rb-y lt-y)))))

  (place-add
   (make-Place lt-x lt-y w h (make-flvector (* w h) 1.0))
   scan))


;;
;; Utilities
;;

(: coords->index (Integer Integer Integer Integer Natural -> Integer))
(define (coords->index x y p-x p-y p-w)
  (+ (* (- y p-y) p-w) (- x p-x)))

(: flvector-add1! (FlVector Natural -> Void))
(define (flvector-add1! v idx)
  (flvector-set! v idx (unsafe-fl+ (flvector-ref v idx) 1.0)))

(: flvector-sub1! (FlVector Natural -> Void))
(define (flvector-sub1! v idx)
  (flvector-set! v idx (unsafe-fl- (flvector-ref v idx) 1.0)))

(: flvector-copy (FlVector -> FlVector))
(define (flvector-copy v)
  (define len (unsafe-flvector-length v))
  (define new (make-flvector len 0.0))
  (for ([i (in-range len)])
       (unsafe-flvector-set! new i (unsafe-flvector-ref v i)))
  new)

(provide
 place-ref
 place-has-point?
 place-ll

 place-add
 place-remove

 grid-scan->place)