#lang scheme
(define (build-syracuse-sequence a0 n)
  (define (iter a k sequence)
    (cond
      [(= k (- n 1)) sequence]
      [(even? a) (iter (/ a 2) (+ k 1) (append sequence (list(/ a 2))))]
      [else (iter (+ (* 3 a) 1) (+ k 1) (append sequence  (list(+ (* 3 a) 1))))]))
  (iter a0 0 (list a0)))
(build-syracuse-sequence 3 5)
      