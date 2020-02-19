#lang scheme
(define (horner-scheme m x)
  (foldl (lambda [k r]
           (+ k (* r x))) 0 m))

(define (polyder coef-lst)
  (define pow (length coef-lst))
  (define pow-lst (cdr (build-list pow (lambda [x]
                                         (- pow x)))))
  (define new-coef-lst (take coef-lst (- pow 1)))
  (if (empty? new-coef-lst)
      '(0)
      (map (lambda [x y]
             (* x y)) pow-lst new-coef-lst)))

(define (newton-method x polynom eps)
  (if (< (abs (horner-scheme polynom x)) eps)
      x
      (newton-method (- x (/ (horner-scheme polynom x) (horner-scheme (polyder polynom) x)))
                     polynom eps)))
  

