#lang scheme
(define (count-digits n)
  (if (< n 10) 1 (+ 1 (count-digits (quotient n 10)))))

(define (reverse-number n)
  (define digits (- (count-digits n) 1))
    (define (rec digits n)
      (if (= digits 0)
          n
          (+ (* (remainder n 10) (expt 10 digits)) (rec (- digits 1) (quotient n 10)))))
    (rec digits n))

(define (full-mirror? lst1 lst2)
  (define (get-full-mirror lst full_mirror)
    (if (empty? lst)
        full_mirror
        (get-full-mirror (cdr lst) (cons (reverse-number (car lst)) full_mirror))))
  (equal? (get-full-mirror lst1 '()) lst2))
    

