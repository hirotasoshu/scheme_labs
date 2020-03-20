#lang scheme
(define (data tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (map-tree func . lst-trees)
  (cond
    [(empty? (car lst-trees)) '()]
    [else
     (list
      (apply func (foldl
                   (lambda [tree result]
                     (cons (data tree) result))
                   '() (reverse lst-trees)))
      (apply map-tree func (foldl
                            (lambda [tree result]
                              (cons (left-branch tree) result))
                            '() (reverse lst-trees)))
      (apply map-tree func (foldl
                            (lambda [tree result]
                              (cons (right-branch tree) result))
                            '() (reverse lst-trees))))]))