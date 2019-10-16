#lang racket
(define (max_digit number)
  (if (< number 10) number (max (remainder number 10) (max_digit (quotient number 10)))))
(max_digit 156)
(max_digit 3870)
(max_digit 913)