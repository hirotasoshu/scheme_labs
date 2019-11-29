#lang scheme
(define (sum lst)
  (foldl (lambda [el result]
           (+ result (abs el))) 0 lst))
(define (diagonally-dominant-matrix? matrix)
   (cdr (foldl (lambda [row result-pair]
                (if (car result-pair)
                    (let* ([drop_row (drop row (car result-pair))]
                           [take_row (reverse (take row (car result-pair)))]
                           [diag_elem (car take_row)]
                           [non_diag_sum (sum (append
                                               (cdr take_row) drop_row))])
                      (if (> (abs diag_elem) non_diag_sum)
                          (cons (+ (car result-pair) 1) #t)
                          (if (= non_diag_sum (abs diag_elem))
                              (cons (+ (car result-pair) 1) (cdr result-pair))
                              (cons #f #f))))
                    result-pair)) (cons 1 #f) matrix)))
                 
                 
                 
  
