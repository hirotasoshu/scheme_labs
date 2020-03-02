#lang scheme
(define (data tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (format-expanded-tree lst)
  (define sorted-lst (sort lst < #:key cdr))
  (define (iter-lst lst i curr-result result)
    (if (empty? lst)
        (if (empty? curr-result)
            (reverse result)
            (reverse (cons curr-result result)))
        (if (eq? i (cdar lst))
            (iter-lst (cdr lst) i (append curr-result (list (caar lst))) result)
            (iter-lst (cdr lst) (cdar lst) (list (caar lst)) (cons curr-result result)))))
  (iter-lst (cdr sorted-lst) (cdar lst) (list (caar lst)) '()))

(define (expand-tree tree)
  (define (iter-tree tree result i)
    (let ([ltree (left-branch tree)]
          [rtree (right-branch tree)])
      (if (empty? rtree)
          (if (empty? ltree)
              result
              (iter-tree (left-branch tree) (append result (list (cons (data ltree) i))) (+ i 1)))
          (if (empty? ltree)
              (iter-tree (right-branch tree) (append result (list (cons (data rtree) i))) (+ i 1))
              (append (iter-tree (left-branch tree) (append result (list (cons (data ltree) i))) (+ i 1))
                      (iter-tree (right-branch tree) (append result (list (cons (data rtree) i))) (+ i 1)))))))
  (if (empty? tree)
      #f
      (cons (list (data tree)) (format-expanded-tree (iter-tree tree '() 2)))))
    
    
