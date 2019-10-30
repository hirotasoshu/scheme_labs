#lang scheme
(define (build_list lst)
  (define (iter lst_ new_lst count)
    (cond
      [(empty? lst_) (if (= count 0) (iter (cdr lst) new_lst 1) new_lst)]
      [else (iter (cdr lst_) (cons (car lst_) new_lst) count)]))
  (if (empty? lst) '() (iter (reverse lst) '() 0)))