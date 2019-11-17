#lang scheme
(define (calc nums funcs)
  (foldl (lambda (num func result)
           ((eval func) result num))
         (car nums) (cdr nums) funcs))
    
    
    
    
