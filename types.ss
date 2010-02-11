#lang typed-scheme

(define-type-alias Point (Vectorof Number))
(define-type-alias Scan (Vectorof Point))

(define-type-alias Grid-Point (Vectorof Integer))
(define-type-alias Grid-Scan (Vectorof Grid-Point))

;; A Prior 
;;
;; x is the maximum extent of the Prior in the x direction
;; y is the maximum extent in the y direction
;; points are the grid points in the Prior
;;
;; Points are ordered row-major
(define-struct: Prior ([x : Integer] [y : Integer] [points : (Vectorof Number)]))

(provide
 (all-defined-out))