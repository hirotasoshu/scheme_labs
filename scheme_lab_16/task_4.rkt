#lang scheme
(define left-branch car)
(define right-branch cdr)
(define (leaf? tree)
  (equal? (left-branch tree) 'leaf))

(define (height tree)
  (if (or (empty? tree) (leaf? tree))
      1
      (* 2 (max (height (left-branch tree)) (height (right-branch tree))))))

(define (frequency tree)
  (define freq (* (height tree) 2))
  (define (iter tree freq)
    (if (empty? tree)
        '()
        (if (leaf? tree)
            (list (cons (right-branch tree) freq))
            (append (iter (left-branch tree) (/ freq 2))
                    (iter (right-branch tree)  (/ freq 2))))))
  (iter tree freq))

            
    