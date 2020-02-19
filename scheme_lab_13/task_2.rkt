#lang scheme
(define (create-list-with-zeros n)
  (build-list (abs n) (lambda [x] (* 0 x))))
(define (polysum coef-lst1 coef-lst2)
  (define pow1 (length coef-lst1))
  (define pow2 (length coef-lst2))
  (define diff (- pow1 pow2))
  (cond
    [(= diff 0)
     (map (lambda [x y] (+ x y)) coef-lst1 coef-lst2)]
    [(< diff 0)
      (map (lambda [x y] (+ x y))
           (append (create-list-with-zeros diff) coef-lst1) coef-lst2)]
    [else
     (map (lambda [x y] (+ x y))
          coef-lst1 (append (create-list-with-zeros diff) coef-lst2))]))
(define (several-polysum . args)
  (foldl (lambda [el res]
           (polysum el res)) (car args) (cdr args)))
     
