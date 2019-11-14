#lang racket
(define (all-sublists lst)
  (define (iter lst prevsub sublists)
    (if (empty? lst)
        (map reverse (reverse sublists))
        (let ([new_sub (cons (car lst) prevsub)])
          (iter (cdr lst) new_sub (cons new_sub sublists)))))
  (iter lst '() '()))
        
    