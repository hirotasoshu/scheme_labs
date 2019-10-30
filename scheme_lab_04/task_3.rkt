#lang scheme
(define (find_neighbor lst)
  (define (iter prev lst)
    (let ((cdr_lst (cdr lst)) (car_lst (car lst)))
      (if (and (>= car_lst prev) (>= car_lst (if (empty? cdr_lst) -inf.0 (car cdr_lst))))
          car_lst
          (iter car_lst cdr_lst))))
  (iter -inf.0 lst))
  
