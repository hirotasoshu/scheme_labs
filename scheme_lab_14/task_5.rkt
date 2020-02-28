#lang scheme
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))


(define (nodes-with-two-child? tree)
  (if (empty? tree)
      #f
      (if (and
           (empty? (left-branch tree))
           (empty? (right-branch tree)))
          #t
          (and (nodes-with-two-child? (left-branch tree))
               (nodes-with-two-child? (right-branch tree))))))
      
