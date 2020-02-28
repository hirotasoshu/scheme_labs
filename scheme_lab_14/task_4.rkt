#lang scheme
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (count-leaf tree)
  (if (empty? tree)
      0
      (if (and (empty? (left-branch tree))
               (empty? (right-branch tree)))
          1
          (+ (count-leaf (left-branch tree)) (count-leaf (right-branch tree))))))

  