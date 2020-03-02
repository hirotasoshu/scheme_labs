#lang scheme
(define (data tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (min-leaf tree)
  (define (iter-tree tree prev_node)
    (if (empty? tree)
        prev_node
        (min (iter-tree (left-branch tree) (data tree)) (iter-tree (right-branch tree) (data tree)))))
  (iter-tree tree +inf.0))
        
