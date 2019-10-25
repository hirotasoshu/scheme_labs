#lang scheme
(define (list-with lst idx val)
  (if (null? lst)
      lst
      (cons
       (if (zero? idx)
           val
           (car lst))
       (list-with (cdr lst) (- idx 1) val))))

(define (true? el)
  (eq? (cdr el) #t))

(define (sieve n)
  (define prime (build-list (+ n 1) (lambda (x) (cons x (< 1 2)))))
  (define (while p prime)
    (define (for i step prime)
      (if (<= i n)
          [for (+ i step) step (list-with prime i (cons i #f))]
          [while (+ p 1) prime]))
    (cond
      [(> (sqr p) n) (list-tail prime 2)]
      [(true? (list-ref prime p)) (for (* p p) p prime)]
      [else (while (+ p 1) prime)]))
  (while 2 prime))

(define (primes-to n)
  (map (lambda (el) (car el)) (filter true? (sieve n))))

(define (simples-product? n)
  (define primes (primes-to n))
  (define (iter i)
    (cond
      [(= i (length primes)) #f]
      [(ormap (lambda (x) (= x (/ n (list-ref primes i)))) primes) #t]
      [else (iter (+ i 1))]))
  (iter 0))
(simples-product? 6)
(simples-product? 8)
(simples-product? 25)
(simples-product? 24)  

