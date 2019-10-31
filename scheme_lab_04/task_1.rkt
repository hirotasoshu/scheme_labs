#lang scheme
(define (return-pair x k)
  (let ((a (expt 10 k)))
    (cons (quotient x a) (remainder x a))))
