#lang scheme
(define (counter lst)
  (define (iter-elem prev counter lst answer)
    (if (empty? lst)
        (cons (cons prev counter) answer)
        (if (= (car lst) prev)
            (iter-elem prev (+ counter 1) (cdr lst) answer)
            (iter-elem (car lst) 1 (cdr lst) (cons (cons prev counter) answer)))))
  (cdr (reverse (iter-elem -inf.0 0 (sort lst <) '()))))
            
  