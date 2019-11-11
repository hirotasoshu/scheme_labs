#lang scheme
(define (elem-counter lst elem)
  (foldl (lambda (a result)
           (if (= a elem) (+ result 1) result)) 0 lst))
(define (max-elem-counter lst)
  (car (sort lst (lambda (x y) (> (elem-counter lst x) (elem-counter lst y))))))
  