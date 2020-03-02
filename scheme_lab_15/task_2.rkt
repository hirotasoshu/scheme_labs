#lang scheme
(define (data tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-symmetric-tree tree)
  (if (empty? tree)
      tree
      (cons (data tree) (cons (make-symmetric-tree (right-branch tree)) (list  (make-symmetric-tree (left-branch tree)))))))
      
  
