#lang scheme
(define (all-diff? lst)
  (define (iter_lst lst)
    (define (iter_elem elem lst_)
      (if (empty? lst_)
          (iter_lst (cdr lst))
          (if (= elem (car lst_))
              #f
              (iter_elem elem (cdr lst_)))))
    (if (empty? lst)
        #t
        (iter_elem (car lst) (cdr lst))))
  (iter_lst lst))
              
          
      