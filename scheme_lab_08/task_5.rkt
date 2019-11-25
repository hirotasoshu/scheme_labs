#lang scheme
(define (make-step row ind)
  (cons (car (drop row (- ind 1))) (take row (- ind 1))))
(define (cycle-matrix n)
  (define ind (- n 1))
  (define first_row
    (build-list  ind (lambda [n] (+ n 1))))
  (reverse (cdr (foldl (lambda [first_row result]
           (cons (make-step (car result) ind) result))
         (list first_row) first_row))))  
    
  

