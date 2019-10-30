#lang scheme
(define (all_even? lst)
  (andmap even? lst))
(all_even? '(6 10 16))
(all_even? '(15 18 13))