#lang scheme
(define (divided-by-index lst)
  (define (iter lst index new_lst)
    (if (empty? lst)
        (reverse new_lst)
        (let ([car_lst (car lst)] [cdr_lst (cdr lst)])
          (if (= (remainder car_lst index) 0)
              (iter cdr_lst (+ index 1) (cons car_lst new_lst))
              (iter cdr_lst (+ index 1) new_lst)))))
  (iter lst 1 '()))
              
        
