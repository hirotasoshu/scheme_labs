#lang scheme
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relative-prime? a b)
  (= (gcd a b) 1))

(define (find-relative-primes lst1 lst2)
  (define (iter-lst lst1 lst2 answer)
    (if (or (empty? lst1) (empty? lst2))
        (reverse answer)
        (if (relative-prime? (car lst1) (car lst2))
            (iter-lst (cdr lst1) (cdr lst2) (cons (cons (car lst1) (car lst2)) answer))
            (iter-lst (cdr lst1) (cdr lst2) answer))))
  (iter-lst lst1 lst2 '()))