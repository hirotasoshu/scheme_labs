#lang scheme
(define (all_even? lst)
  (if (empty? lst) #t
  (and (even? (car lst)) (all_even? (cdr lst)))))
