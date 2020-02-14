#lang scheme
(define (int->list n)
  (define (f m lst)
    (if(= m 0)
       lst
       (f (quotient m 10) (cons (remainder m 10) lst))))
  (f n null))

(define (list->int lst)
  (define (iter lst n)
    (if (empty? lst)
        n
        (iter (cdr lst) (+ (* 10 n) (car lst)))))
  (iter lst 0))

(define (div? a b)
  (= (remainder a b) 0))
(define (prime? number)
  (define (iter i)
    (cond
      [(= number 1) #f]
      [(= number 2) #t]
      [(even? number) #f]
      [(> (sqr i) number) #t]
      [(div? number i) #f]
      [else (iter (+ i 2))]))
  (iter 3))

(define (max-prime n)
  (define lst (int->list n))
  (define (iter-lst primes j)
    (define (iter-num i flst slst primes)
      (define num (list->int (append (append flst (list i)) slst)))
        (if (> i 9)
            primes
            (if (prime? num)
                (iter-num (+ i 1) flst slst (cons num primes))
                (iter-num (+ i 1) flst slst primes))))
    (define flst (take lst j))
    (define slst (drop lst j))
    (if (empty? slst)
        (append primes (iter-num 0 flst slst '()))
        (iter-lst (append primes (iter-num 0 flst slst '())) (+ j 1))))
    (let ([result (iter-lst '() 0)])
      (if (empty? result)
          #f
          (apply max result))))
    
    
    
  
