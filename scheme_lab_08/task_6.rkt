#lang scheme
(define (count-zeros-rows matrix)
  (foldl (lambda (row count)
           (if (andmap zero? row)
                (+ count 1)
                count)) 0 matrix))
