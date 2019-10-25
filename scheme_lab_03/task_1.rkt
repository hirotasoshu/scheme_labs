#lang scheme
(define (div? a b)
  (= (remainder a b) 0))
(define (divided_by_all_digits? number)
  (define (iter n)
    (cond
      [(= (remainder n 10) 0) #f]
      [(< n 10) (div? number n)]
      [(div? number (remainder n 10)) (iter (quotient n 10))]
      [else #f]))
  (iter number))
(divided_by_all_digits? 1000)
(divided_by_all_digits? 336)
(divided_by_all_digits? 2224)
(divided_by_all_digits? 4444)
(divided_by_all_digits? 445)
(divided_by_all_digits? 0)
    
    