#lang scheme
(define (check_eq a b)
  (if (= a b) 0 1))
(define (eq_digits? num)
  (define (iter digit n sum)
    (if (< n 10)
        (= (+ sum (check_eq n digit)) 0)
        (iter digit (quotient n 10) (+ sum (check_eq (remainder n 10) digit)))))
  (iter (remainder num 10) num 0))
(eq_digits? 222)
(eq_digits? 322)
 