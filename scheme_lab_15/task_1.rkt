#lang scheme
(define (data tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (find-dividing-node tree)
  (define (iter-tree tree prev_node)
    (if (empty? tree)
        #f
        (let ([node (data tree)])
          (if (and (not (= node 0)) (= (remainder prev_node node) 0))
              prev_node
              (or (iter-tree (left-branch tree) node)
                  (iter-tree (right-branch tree) node))))))
  (if (empty? tree)
        #f
  (or (iter-tree (left-branch tree) (data tree))
                  (iter-tree (right-branch tree) (data tree)))))
  
      
                 