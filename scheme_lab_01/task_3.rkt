#lang scheme
(define dozens
  (list "" "десять" "двадцать " "тридцать " "сорок " "пятьдесят " "шестьдесят " "семьдесят " "восемьдесят " "девяносто " "сто"))
(define digits
  (list "" "один" "два" "три" "четыре" "пять" "шесть" "семь" "восемь" "девять" "десять"))
(define range_10_20
  (list "одиннадцать" "двенадцать" "тринадцать" "четырнадцать" "пятнадцать" "шестнадцать" "семнадцать" "восемнадцать" "девятнадцать"))
(define (in_range_10_20? number)
  (and (> number 10) (< number 20)))
(define (div_10 number)
  (quotient number 10))
(define (mod_10 number)
  (remainder number 10))
(define (letter_format number)
  (cond
    [(in_range_10_20? number) (list-ref range_10_20 (remainder number 11))]
    [else (string-append (list-ref dozens (div_10 number)) (list-ref digits (mod_10 number)))]))
(letter_format 34)
(letter_format 69)
(letter_format 11)
(letter_format 15)
(letter_format 20)
(letter_format 5)
(letter_format 100)