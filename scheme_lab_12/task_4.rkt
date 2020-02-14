#lang scheme
(define (empty-string? str)
  (not (non-empty-string? str)))

(define (get-nums str)
  (define str-lst (string->list str))
  (define dig-lst '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define (iter-str str-lst substr substrs)
    (if (empty? str-lst)
        (if (empty-string? substr)
        (reverse substrs)
        (reverse (cons substr substrs)))
        (if (not (member (car str-lst) dig-lst))
            (if (empty-string? substr)
                (iter-str (cdr str-lst) "" substrs)
                (iter-str (cdr str-lst) "" (cons substr substrs)))
            (iter-str (cdr str-lst) (string-append substr (string (car str-lst))) substrs))))
  (iter-str str-lst "" '()))

(define (chr->dig chr)
  (cond
    [(equal? chr #\0) 0]
    [(equal? chr #\1) 1]
    [(equal? chr #\2) 2]
    [(equal? chr #\3) 3]
    [(equal? chr #\4) 4]
    [(equal? chr #\5) 5]
    [(equal? chr #\6) 6]
    [(equal? chr #\7) 7]
    [(equal? chr #\8) 8]
    [(equal? chr #\9) 9]))

(define (str->num str)
  (define dig-lst (map chr->dig (string->list str)))
  (foldl (lambda [el res]
           (+ (* res 10) el)) 0 dig-lst))

(define (str->sum str)
  (apply + (map str->num (get-nums str))))