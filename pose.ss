#lang typed/scheme

(require
 scheme/match
 "types.ss")

(: pose-x (Pose -> Real))
(define (pose-x pose)
  (vector-ref pose 0))

(: pose-y (Pose -> Real))
(define (pose-y pose)
  (vector-ref pose 1))

(: pose-a (Pose -> Real))
(define (pose-a pose)
  (vector-ref pose 2))

(: pose< (Pose Pose -> Boolean))
;; A pose p1 is less than a pose 2 if has less rotation
;; or equal rotation and less translation
(define (pose< p1 p2)
  (match-define (vector x1 y1 a1) p1)
  (match-define (vector x2 y2 a2) p2)

  (cond
   [(< a1 a2) #t]
   [(= a1 a2) (< (+ (* x1 x1) (* y1 y1)) (+ (* x2 x2) (+ y2 y2)))]
   [else #f]))


(provide
 pose-x
 pose-y
 pose-a

 pose<)