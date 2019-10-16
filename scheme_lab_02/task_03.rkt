#lang scheme
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
(define (check_fact? number)
  (define (iter n)
    (if (>= (factorial n) number ) (= (factorial n) number)
        (iter (+ n 1))))
  (iter 1))
(check_fact? 6)
(check_fact? 24)
(check_fact? 25)
        
