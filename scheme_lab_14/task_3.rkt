#lang scheme
(define (count-max-elem tree)
  (define lst (flatten tree))
  (define (count-max max counter lst)
    (if (empty? lst)
        (if (= counter 0)
            #f
            counter)
        (let ([el (car lst)])
          (if (> el max)
              (count-max el 1 (cdr lst))
              (if (= el max)
                  (count-max max (+ counter 1) (cdr lst))
                  (count-max max counter (cdr lst)))))))
  (count-max -inf.0 0 lst))
  