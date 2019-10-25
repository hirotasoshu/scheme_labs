#lang scheme
(define (get_digit n)
  (define (while str i)
    (if (>= (string-length str) n)
        [string->number (string (string-ref str (- n 1)))]
        [while (string-append str (number->string (+ i 1))) (+ i 1)]))
  (while "1" 1))
(get_digit 2)
(get_digit 9)
(get_digit 10)
(get_digit 11)