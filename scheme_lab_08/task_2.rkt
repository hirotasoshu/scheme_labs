#lang scheme
(define (positive-matrix? matrix)
  (andmap (lambda (row)
            (andmap positive? row)) matrix))
  
  