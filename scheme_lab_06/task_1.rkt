#lang scheme
(define (count-diff lst)
  (define (iter-elem lst prev counter)
    (if (empty? lst)
        counter
        (if (= prev (car lst))
            (iter-elem (cdr lst) prev counter)
            (iter-elem (cdr lst) (car lst) (+ counter 1)))))
  (iter-elem (sort lst <) -inf.0 0))
    
 
