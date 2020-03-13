#lang racket
(define (left tree)
  (if (empty? tree)
      tree
      (car tree)))
(define (right tree)
  (if (empty? tree)
      tree
      (cdr tree)))
(define (leaf? tree)
  (equal? (left tree) 'leaf))

(define (height tree result)
  (if (empty? tree)
      (if result
          (reverse result)
          #f)
      (if (leaf? tree)
          #f
      (let ([lresult (height (left tree) (cons 0 result))]
        [rresult (height (right tree) (cons 1 result))])
        (if lresult
            (if rresult
                (if (< (length lresult) (length rresult))
                    lresult
                    rresult)
                lresult)
            rresult)))))

(define (add tree smbl pth)
  (if (empty? pth)
      (cons 'leaf smbl)
      (if (= 1 (car pth))
          (cons (left tree)
                (add (right tree)
                     smbl
                     (cdr pth)))
          (cons (add (left tree)
                     smbl
                     (cdr pth))
                (right tree)))))
(define (build-tree lst)
  (foldl (lambda [pair tree]
           (add tree (car pair) (cdr pair))) '() lst))

(define (get-min-code lst)
  (define tree (build-tree lst))
  (height tree '()))

(define (reduce-code code)
  (define (iter code1)
    (cond [(empty? code1) #f]
          [(equal? (cdar code1) (get-min-code (remove (car code1) code))) (iter (cdr code1))]
          [else (caar code1)]))
  (iter code))