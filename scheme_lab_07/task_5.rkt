#lang scheme
(require racket/format)

(define (account-status sum percent protocol)
  (define days (build-list 30 add1))
  (define days-in-protocol (remove-duplicates (map (lambda (record)
                                                     (car record)) protocol)))                          
  (define percent_mult (+ 1 (/ percent 100)))
  (define (get-sum-of-day sum day)
    (define day-record (filter (lambda (record) (= (car record) day)) protocol))
    (* (foldl (lambda (record sum)
                ((eval (cadr record)) sum (caddr record))) sum day-record) percent_mult))
  (foldl (lambda (day sum)
           (if (member day days-in-protocol)
               (get-sum-of-day sum day)
               (* sum percent_mult))) sum days))

(define (display-account-status sum percent protocol)
  (display (~r (account-status sum percent protocol) #:precision 5)))
             
