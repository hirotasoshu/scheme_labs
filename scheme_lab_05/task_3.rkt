#lang scheme
(define (max-parity lst)
  (define (iter lst curr_count is_even max_count)
    (cond
      [(empty? lst) (max curr_count max_count)]
      [(and (even? (car lst)) (eq? is_even #t))
            (iter (cdr lst) (+ curr_count 1) #t max_count)]
      [(even? (car lst)) (iter (cdr lst) 1 #t (max curr_count max_count))]
      [(eq? is_even #f) (iter (cdr lst) (+ curr_count 1) #f max_count)]
      [else (iter (cdr lst) 1 #f (max max_count curr_count))]))
    (iter lst 0 #f 0))
      