#lang typed/scheme

(require
 scheme/fixnum
 scheme/vector
 "base.ss"
 "types.ss"
 "grid.ss"
 "point.ss"
 "util.ss")

(: grid-scan-transform/pose (Grid-Scan Pose -> Grid-Scan))
(define (grid-scan-transform/pose scan pose)
  (grid-scan-transform scan
                       (vector-ref pose 0)
                       (vector-ref pose 1)
                       (vector-ref pose 2)))

(: grid-scan-transform (Grid-Scan Real Real Real -> Grid-Scan))
(define (grid-scan-transform scan xt yt a)
  (vector-map (lambda: ([pt : Grid-Point]) 
                (grid-point-transform pt xt yt a))
              scan))

(: grid-point-transform (Grid-Point Real Real Real -> Grid-Point))
(define (grid-point-transform pt xt yt a)
  (define cosa (assert (number->real (cos a))))
  (define sina (assert (number->real (sin a))))
  (define x (point-x pt))
  (define y (point-y pt))

  (vector
   (assert (number->exact-integer
            (floor (+ (- (* cosa x) (* sina y)) xt))))
   (assert (number->exact-integer
            (floor (+ (+ (* sina x) (* cosa y)) yt))))))

(: grid-scan-bb (Grid-Scan -> (values Grid-Point Grid-Point)))
;; Calculate the bounding box for a grid scan, returning the
;; top left and bottom right point
(define (grid-scan-bb scan)
  (define start-pt (vector-ref scan 0))
  (define start-x (point-x start-pt))
  (define start-y (point-y start-pt))
  (let loop ([lt-x start-x] [lt-y start-y]
             [rb-x start-x] [rb-y start-y]
             [idx 0])
    (if (fx= idx (vector-length scan))
        (values (vector lt-x lt-y) (vector rb-x rb-y))
        (let* ([pt (vector-ref scan idx)]
               [x  (point-x pt)]
               [y  (point-y pt)])
          (loop
           (if (fx< x lt-x) x lt-x)
           (if (fx< y lt-y) y lt-y)
           (if (fx< rb-x x) x rb-x)
           (if (fx< rb-y y) y rb-y)
           (add1 idx))))))



(provide
 grid-scan-transform
 grid-scan-transform/pose
 grid-scan-bb)
