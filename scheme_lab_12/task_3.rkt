#lang scheme
(define (check-deg a b)
  (define min_ (min a b))
  (define max_ (max a b))
  (define rem (remainder max_ min_))
  (define div (/ max_ min_))
  (if (< min_ 2)
      #f
      (if (= rem 0)
          (if (= div 1)
              min_
              (check-deg div min_))
          #f)))
      
      