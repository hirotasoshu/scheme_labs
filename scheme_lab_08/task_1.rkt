#lang scheme
(define (sum-matrix matrix1 matrix2)
  (map (lambda (row1 row2)
         (map + row1 row2)) matrix1 matrix2))
   