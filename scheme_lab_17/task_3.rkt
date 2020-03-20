#lang scheme
(define (data tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (leaf? tree)
  (and (empty? (left-branch tree)) (empty? (right-branch tree))))

(define (remove-chains tree)
  (cond
    [(leaf? tree) tree]
    [(and (not (empty? (left-branch tree))) (empty? (right-branch tree))) (remove-chains (left-branch tree))]
    [(and (not (empty? (right-branch tree))) (empty? (left-branch tree))) (remove-chains (right-branch tree))]
    [else (list (data tree) (remove-chains (left-branch tree)) (remove-chains (right-branch tree)))]))
