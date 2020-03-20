#lang scheme
(define (data tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (leaf? tree)
  (and (empty? (left-branch tree)) (empty? (right-branch tree))))

(define (foldl-tree_ func result initial-result . lst-trees) ;основная функция (костыль, поэтому снизу обертка)
  ;initial-result -- костыль, когда оба поддерева непустые (result два раза использовался в вычислениях)
  (cond
    [(leaf? (car lst-trees)) ;если лист
     (apply func (cons result
                       (foldl (lambda [tree result]
                                (cons (data tree) result))
                              '() (reverse lst-trees))))] 
    [(and ;пустое левое поддерево
      (not (empty? (left-branch (car lst-trees))))
      (empty? (right-branch (car lst-trees))))
     (apply foldl-tree_ func
            (apply func
                   (cons result (foldl (lambda [tree result]
                                         (cons (data tree) result))
                                       '() (reverse lst-trees))))
            initial-result
            (foldl (lambda [tree result]
                     (cons (left-branch tree) result))
                   '() (reverse lst-trees)))]
    [(and ;пустое правое поддерево
      (not (empty? (right-branch (car lst-trees))))
      (empty? (left-branch (car lst-trees))))
     (apply foldl-tree_ func
            (apply func
                   (cons result (foldl (lambda [tree result]
                                         (cons (data tree) result))
                                       '() (reverse lst-trees))))
            initial-result
            (foldl (lambda [tree result]
                     (cons (right-branch tree) result))
                   '() (reverse lst-trees)))]
    [else
     (func
      (apply foldl-tree_ func
             (apply func
                    (cons result (foldl (lambda [tree result]
                                          (cons (data tree) result))
                                        '() (reverse lst-trees))))
             initial-result
             (foldl (lambda [tree result]
                      (cons (left-branch tree) result))
                    '() (reverse lst-trees)))
      (apply foldl-tree_ func
             initial-result
             initial-result
             (foldl (lambda [tree result]
                      (cons (right-branch tree) result))
                    '() (reverse lst-trees))))]))

(define (foldl-tree func initial-result . lst-trees) ;обертка над основной функцией
  (apply foldl-tree_ func initial-result initial-result lst-trees))

