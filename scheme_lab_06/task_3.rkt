#lang scheme
(define (diff_fib_nums lst)
  (define (count_fib_nums lst prev curr counter was_one)
    (define (isFib? prev curr num)
      (if (< curr num)
          (isFib? curr (+ prev curr) num)
          (if (= curr num)
              (count_fib_nums (cdr lst) prev curr (+ counter 1) was_one)
              (count_fib_nums (cdr lst) (- curr prev) prev counter was_one))))
    (if (empty? lst)
        counter
        (if (= curr (car lst))
            (if (and (= curr 1) (eq? was_one #f))
                (count_fib_nums (cdr lst) prev curr (+ counter 1) #t)
                (count_fib_nums (cdr lst) prev curr counter was_one))
            (isFib? prev curr (car lst)))))
    (count_fib_nums (sort lst <) 1 1 0 #f))


      
              
                              
      
      
    
