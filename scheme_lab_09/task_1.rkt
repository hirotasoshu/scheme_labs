#lang scheme
(define (count-zeros row)
  (foldl (lambda [elem counter]
           (if (zero? elem)
               (+ counter 1)
               counter)) 0 row))
(define (find-row-with-max-zeros matrix)
  (cdr (foldl (lambda [row result]
                   (let ([curr_counter (count-zeros row)]
                         [max_counter (car result)])
                     (if (> curr_counter max_counter)
                         (cons curr_counter row)
                         result))) (cons 0 "There's no zero rows") matrix)))