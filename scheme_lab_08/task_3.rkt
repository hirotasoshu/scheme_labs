#lang scheme
(define (lower-triangular-matrix? matrix)
  (if (foldl (lambda [row counter]
               (if counter
                   (if (andmap zero? (list-tail row counter))
                       (+ counter 1)
                       #f)
                   #f)) 1 matrix)
      #t
      #f))
  
