#lang scheme
(define (build-sublists lst)
  (let* ([reverse_lst (reverse lst)] [car_lst (car reverse_lst)] [cdr_lst (cdr reverse_lst)])
    (foldl (lambda (elem answer)
             (if (= (length answer) 1)
                 (cons (cons elem answer) (cons answer '()))
                 (cons (cons elem (car answer)) answer))) 
             (cons car_lst '()) cdr_lst)))

           
         
  
