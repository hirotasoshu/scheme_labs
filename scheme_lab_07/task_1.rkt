#lang scheme
(define (get-lst-idx lst)
  (map (lambda (elem idx)
         (cons elem idx))
       lst (build-list (length lst) (lambda (x) (+ x 1)))))
(define (divided-by-idx lst)
  (filter (lambda (el) (= (remainder (car el) (cdr el)) 0)) (get-lst-idx lst)))
  
