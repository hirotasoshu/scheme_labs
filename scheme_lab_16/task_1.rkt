#lang scheme

(define (not-leaf? tree)
  (not (equal? (left tree) 'leaf)))

(define (left tree)
  (if (or (empty? tree) (not tree))
      tree
      (car tree)))
(define (right tree)
  (if (or (empty? tree) (not tree))
      tree
      (cdr tree)))

(define (add tree smbl pth)
  (if (not-leaf? tree)
  (if (empty? pth)
      (if (and (empty? (right tree)) (empty? (left tree)))
          (cons 'leaf smbl)
          #f)
          (if (= 1 (car pth))
              (let ([add_right (add (right tree)
                         smbl
                         (cdr pth))])
                (if add_right
                    (cons (left tree) add_right)
                    #f))
              (let ([add_left (add (left tree)
                         smbl
                         (cdr pth))])
                (if add_left
                    (cons add_left (right tree))
                    #f))))
  #f))

(define (Fano-condition? lst)
 (if (foldl (lambda
             [pair tree]
              (if tree
                  (add tree (car pair) (cdr pair))
                  #f)) '() lst)
     #t
     (if (foldl (lambda
             [pair tree]
              (if tree
                  (add tree (car pair) (cdr pair))
                  #f)) '() (map (lambda [x]
           (cons (car x) (reverse (cdr x)))) lst))
         #t
         #f)))
  
     
  