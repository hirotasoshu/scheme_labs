#lang scheme
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
  
  