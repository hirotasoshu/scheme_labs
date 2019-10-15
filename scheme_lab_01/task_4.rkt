#lang scheme
(define (same_space? x1 y1 x2 y2)
  ( = (remainder (+ x1 x2 y1 y2) 2) 0))
(define (same_diag? x1 y1 x2 y2)
  ( = (abs (- x1 x2)) (abs (- y1 y2))))
(define (move_count x1 y1 x2 y2)
  (if (same_space? x1 y1 x2 y2)
      (cond
        [(same_diag? x1 y1 x2 y2) 1]
        [else 2])
      -1))
(move_count 3 3 4 4)
(move_count 3 3 4 5)
(move_count 5 4 6 7)
