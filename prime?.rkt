#lang scheme
(define (div? a b)
  (= (remainder a b) 0))
(define (prime? number)
  (define (iter i)
    (cond
      [(= number 1) #f]
      [(= number 2) #t]
      [(even? number) #f]
      [(> (sqr i) number) #t]
      [(div? number i) #f]
      [else (iter (+ i 2))]))
  (iter 3))
(prime? 2147483647)       


