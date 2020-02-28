#lang scheme
(define (build-all-triplets lst) ;первая версия, которая выдает все тройки
  (define (build-lists-and-triplets lst lst1 lst2 lst3)
    (define (get-all-triplets lst1 lst2 lst3)
      (define (iter-lst1 lst1 result)
        (define (iter-lst2 lst2 a result)
          (define (iter-lst3 lst3 b result)
            (if (empty? lst3)
                result
                (iter-lst3 (cdr lst3) b (cons (list a b (car lst3)) result))))
          (if (empty? lst2)
              result
              (iter-lst2 (cdr lst2) a (iter-lst3 lst3 (car lst2) result))))
        (if (empty? lst1)
            result
            (iter-lst1 (cdr lst1) (iter-lst2 lst2 (car lst1) result))))
      (iter-lst1 lst1 '()))
    (if (empty? lst)
        (get-all-triplets lst1 lst2 lst3)
        (let* ([el (car lst)] [mod (remainder el 3)])
          (cond
            [(= mod 0) (build-lists-and-triplets (cdr lst) (cons el lst1) lst2 lst3)]
            [(= mod 1) (build-lists-and-triplets (cdr lst) lst1 (cons el lst2) lst3)]
            [(= mod 2) (build-lists-and-triplets (cdr lst) lst1 lst2 (cons el lst3))]))))
  (build-lists-and-triplets (remove-duplicates lst) '() '() '()))

(define (build-triplets lst) ;вторая версия, которая выдает тройки только с уникальными элементами
  (define (build-lists-and-triplets lst lst1 lst2 lst3)
    (define (get-triplets lst1 lst2 lst3 result)
      (if (or (empty? lst1) (empty? lst2) (empty? lst3))
          result
          (get-triplets (cdr lst1) (cdr lst2) (cdr lst3) (cons (list (car lst1) (car lst2) (car lst3)) result))))
    (if (empty? lst)
        (get-triplets lst1 lst2 lst3 '())
        (let* ([el (car lst)] [mod (remainder el 3)])
          (cond
            [(= mod 0) (build-lists-and-triplets (cdr lst) (cons el lst1) lst2 lst3)]
            [(= mod 1) (build-lists-and-triplets (cdr lst) lst1 (cons el lst2) lst3)]
            [(= mod 2) (build-lists-and-triplets (cdr lst) lst1 lst2 (cons el lst3))]))))
  (build-lists-and-triplets (remove-duplicates lst) '() '() '()))
    
    
    
    