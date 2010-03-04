#lang typed/scheme

;; A Point is a vector of two elements: x and y coordinates
(define-type-alias Point (Vectorof Real))
;; A Pose is a vector of three elements: x and y coordinates, and a rotation
(define-type-alias Pose (Vectorof Real))
(define-type-alias Scan (Vectorof Point))

(define-type-alias Grid-Point (Vectorof Integer))
(define-type-alias Grid-Scan (Vectorof Grid-Point))


;; A Sample is a sample from the scan matching procedure,
;; consisting on a log-likelihood and the pose the scan was
;; in to obtain the LL.
(define-type-alias Sample (Pair Real Pose))

(: sample-pose (Sample -> Pose))
(define (sample-pose s)
  (cdr s))

;; A Place represents a physical location in the world. We try to match scans against places
;;
;; x is the x coordinate of the first point in points
;; y is the y coordinate of the first point in points
;; w is the maximum extent of the Place in the x direction (width)
;; h is the maximum extent in the y direction (height)
;; points are the grid points in the Place
;;
;; Points are ordered row-major
(define-struct: Place
  ([x : Integer] [y : Integer] [w : Natural] [h : Natural]
   [points : (Vectorof Real)])
  #:transparent)

(provide
 (all-defined-out))