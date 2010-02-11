#lang typed/scheme

(define-type-alias Point (Vectorof Real))
(define-type-alias Scan (Vectorof Point))

(define-type-alias Grid-Point (Vectorof Integer))
(define-type-alias Grid-Scan (Vectorof Grid-Point))

;; A Place represents a physical location in the world. We try to match scans against places
;;
;; x is the maximum extent of the Place in the x direction
;; y is the maximum extent in the y direction
;; points are the grid points in the Place
;;
;; Points are ordered row-major
(define-struct: Place ([x : Natural] [y : Natural] [points : (Vectorof Real)]))

(provide
 (all-defined-out))