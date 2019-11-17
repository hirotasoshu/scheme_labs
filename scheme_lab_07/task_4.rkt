#lang scheme
(define (build-sublists lst)
  (define (rec lst new-lst)
    (if (empty? lst)
        (reverse new-lst)
        (rec (cdr lst) (cons lst new-lst))))
  (rec lst '()))
