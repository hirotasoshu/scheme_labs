#lang scheme
(define (int->list n)
  (define (f m lst)
    (if(= m 0)
       lst
       (f (quotient m 10) (cons (remainder m 10) lst))))
  (f n null))

(define (list->int lst)
  (define (iter lst n)
    (if (empty? lst)
        n
        (iter (cdr lst) (+ (* 10 n) (car lst)))))
  (iter lst 0))

(define (create-list-with-zeros n)
  (build-list (abs n) (lambda [x] (* 0 x))))
(define (polysum-in-num-system coef-lst1 coef-lst2 k)
  (define pow1 (length coef-lst1))
  (define pow2 (length coef-lst2))
  (define diff (- pow1 pow2))
  (define (sum coef-lst1 coef-lst2)
    (define poly1 (reverse coef-lst1))
    (define poly2 (reverse coef-lst2))
    (define (iter poly1 poly2 result delta)
      (if (empty? poly1)
          (if (= delta 0)
              result
              (cons delta result))
          (let* ([sum (+ (car poly1) (car poly2) delta)] [num (remainder sum k)]
                                                    [new-delta (quotient sum k)])
                 (iter (cdr poly1) (cdr poly2) (cons num result) new-delta))))
    (iter poly1 poly2 '() 0))
  (cond
    [(= diff 0)
     (sum coef-lst1 coef-lst2)]
    [(< diff 0)
      (sum (append (create-list-with-zeros diff) coef-lst1) coef-lst2)]
    [else
     (sum coef-lst1 (append (create-list-with-zeros diff) coef-lst2))]))

(define (sum-in-num-system k . args)
  (define polynoms (map (lambda [n]
         (int->list n)) args))
  (list->int (foldl (lambda [el res]
           (polysum-in-num-system el res k)) (car polynoms) (cdr polynoms))))
  
  
  
