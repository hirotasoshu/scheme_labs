#lang scheme
(define (number-triplet lst)
  (define (find-c a b lst)
    (cond
      [(empty? lst) #f]
      [(= (+ a b) (car lst)) (list a b (car lst))]
      [else (find-c a b (cdr lst))]))
  (define (iter-b a lst)
    (if (empty? lst)
        #f
        (let* ([b (car lst)] [cdr_lst (cdr lst)] [result (find-c a b cdr_lst)])
          (if result
              result
              (iter-b a cdr_lst)))))
  (define (iter-lst lst)
    (if (empty? lst)
        #f
        (let* ([cdr_lst (cdr lst)] [result (iter-b (car lst) cdr_lst)])
          (if result
              result
              (iter-lst cdr_lst)))))
  (iter-lst (sort lst <)))
  
  