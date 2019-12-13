#lang scheme
(define (empty-string? str)
  (not (non-empty-string? str)))
(define (count-words str delim)
  (define str-lst (string->list str))
  (define delim-lst (string->list delim))
  (define (iter-str str-lst substr counter)
    (if (empty? str-lst)
        (if (empty-string? substr)
        counter
        (+ counter 1))
        (if (member (car str-lst) delim-lst)
            (if (empty-string? substr)
                (iter-str (cdr str-lst) "" counter)
                (iter-str (cdr str-lst) "" (+ counter 1)))
            (iter-str (cdr str-lst) (string-append substr (string (car str-lst))) counter))))
  (iter-str str-lst "" 0))
            
            
        
