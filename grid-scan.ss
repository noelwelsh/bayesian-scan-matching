#lang typed/scheme

(require
 "types.ss")

(: grid-scan-transform (Grid-Scan Real Real Real -> Grid-Scan))
(define (grid-scan-transform scan x y a)
  scan)


(provide
 grid-scan-transform)