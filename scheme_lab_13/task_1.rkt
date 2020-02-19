#lang scheme
(define (polyder coef-lst)
  (define pow (length coef-lst))
  (define pow-lst (cdr (build-list pow (lambda [x]
                                         (- pow x)))))
  (define new-coef-lst (take coef-lst (- pow 1)))
  (if (empty? new-coef-lst)
      '(0)
      (map (lambda [x y]
             (* x y)) pow-lst new-coef-lst)))
  
     
  
