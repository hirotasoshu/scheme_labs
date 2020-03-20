#lang scheme
(define (data tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (leaf? tree)
  (and (empty? (left-branch tree)) (empty? (right-branch tree))))
(define (empty-leaf? tree)
  (and (leaf? tree) (empty? (data tree))))
                     
(define (find-root edge-lst)
  (car (ormap
        (lambda [node]
          (if
           (ormap (lambda [child]
                    (equal? (car node) (cdr child))) (remove node edge-lst))
           #f
           node)) edge-lst)))

(define (make-list-branches edge-lst)
  (define (iter-edges lst1 lst2)
    (if (empty? lst1)
        lst2
        (let ([branch (findf (lambda [node]
                               (eq? (caar lst1) (car node)))
                             (remove (car lst1) edge-lst))])
          (iter-edges (remove branch (cdr lst1)) (cons (list (caar lst1)
                                                       (cdar lst1)
                                                       (if branch
                                                           (cdr branch)
                                                           '())) lst2)))))
  (iter-edges edge-lst '()))

(define (make-tree edge-lst)
  (define list-branches (make-list-branches edge-lst))
  (define (iter-tree tree)
    (cond
      [(empty-leaf? tree) '()]
      [(leaf? tree) tree]
      [else
       (let ([left_branch (findf (lambda [child]
                                   (eq? (car child) (left-branch tree))) list-branches)]
             [right_branch (findf (lambda [child]
                                    (eq? (car child) (right-branch tree))) list-branches)])
         (list (data tree)
               (iter-tree (if left_branch
                          left_branch
                          (list (left-branch tree) '() '())))
               (iter-tree (if right_branch
                          right_branch
                          (list (right-branch tree) '() '())))))]))
  (iter-tree (findf (lambda [node]
                  (equal? (car node) (find-root edge-lst))) list-branches))) 