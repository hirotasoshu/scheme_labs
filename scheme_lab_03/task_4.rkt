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
  (define (iter p prime)
    (define (mark_numbers i step prime)
      (if (<= i n)
          [mark_numbers (+ i step) step (list-with prime i (cons i #f))]
          [iter (+ p 1) prime]))
    (cond
      [(> (sqr p) n) (list-tail prime 2)]
      [(true? (list-ref prime p)) (mark_numbers (* p p) p prime)]
      [else (iter (+ p 1) prime)]))
  (iter 2 prime))

(define (primes-to n)
  (map (lambda (el) (car el)) (filter true? (sieve n))))

(define (primes-product? n)
  (define primes (primes-to n))
  (define (iter i)
    (cond
      [(= i (length primes)) #f]
      [(ormap (lambda (x) (= x (/ n (list-ref primes i)))) primes) #t]
      [else (iter (+ i 1))]))
  (iter 0))
(primes-product? 6)
(primes-product? 8)
(primes-product? 25)
(primes-product? 24)  

