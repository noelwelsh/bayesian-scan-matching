#lang typed/scheme

(require
 scheme/vector
 "base.ss"
 "types.ss"
 "grid.ss"
 "point.ss"
 "util.ss")

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

(provide
 grid-scan-transform)