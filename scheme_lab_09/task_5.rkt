#lang scheme
(define (transpose matrix)
  (apply map list matrix))
(define (find-row-equal-to-column matrix)
  (define transposed (transpose matrix))
  (foldl (lambda [row result]
           (if result
               result
               (if (ormap (lambda [col]
                            (equal? row col)) transposed)
                   row
                   #f))) #f matrix))
  
  
