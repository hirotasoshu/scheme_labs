#lang scheme
(define (data tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (count-left-nodes tree)
  (define (iter-tree tree counter)
    (let ([ltree (left-branch tree)]
          [rtree (right-branch tree)])
      (if (empty? rtree)
          (if (empty? ltree)
              counter
              (iter-tree ltree (+ counter 1)))
          (if (empty? ltree)
              (iter-tree rtree counter)
              (+ (iter-tree ltree (+ counter 1)) (iter-tree rtree counter))))))
  (iter-tree tree 0))
          
          

