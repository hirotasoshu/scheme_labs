#lang scheme
(define (three-digits-sum lst)
  (foldl (lambda (a result)
           (if (and (>= a 100) (<= a 999))
               (+ result a)
               result)) 0 lst))
