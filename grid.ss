#lang typed/scheme

(require
 scheme/vector
 "types.ss"
 "util.ss")

;; discretise : Convert value to an integer number of units.
;;
;; E.g. (discretise 101 10) -> 10
(: discretise (Real Real -> Integer))
(define (discretise value unit)
  (assert 
   (number->exact-integer (floor (/ value unit)))))


(: scan->grid-scan (Scan Real -> Grid-Scan))
(define (scan->grid-scan scan unit)
  (: point->grid-point (Point -> Grid-Point))
  (define (point->grid-point point)
    (vector (discretise (vector-ref point 0) unit)
            (discretise (vector-ref point 1) unit)))
  
  (vector-map point->grid-point scan))


(provide
 discretise
 scan->grid-scan)