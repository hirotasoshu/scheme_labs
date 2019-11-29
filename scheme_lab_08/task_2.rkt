#lang scheme
(define (non-negative? num)
  (>= num 0))
(define (non-negative-matrix? matrix)
  (andmap (lambda (row)
            (andmap non-negative? row)) matrix))
  
  