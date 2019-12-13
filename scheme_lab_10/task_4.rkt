#lang scheme
(define (empty-string? str)
  (not (non-empty-string? str)))
(define (get-substrs str delim)
  (define str-lst (string->list str))
  (define delim-lst (string->list delim))
  (define (iter-str str-lst substr substrs)
    (if (empty? str-lst)
        (if (empty-string? substr)
        (reverse substrs)
        (reverse (cons substr substrs)))
        (if (member (car str-lst) delim-lst)
            (if (empty-string? substr)
                (iter-str (cdr str-lst) "" substrs)
                (iter-str (cdr str-lst) "" (cons substr substrs)))
            (iter-str (cdr str-lst) (string-append substr (string (car str-lst))) substrs))))
  (iter-str str-lst "" '()))
(define (equal-words? str)
  (define words (get-substrs str " "))
  (define (iter-words words res)
    (if (empty? words)
        res
        (if res
            res
                (if (ormap (lambda [word]
                             (equal? word (car words))) (cdr words))
                    (car words)
                    (iter-words (cdr words) #f)))))
  (iter-words words #f))
    
  