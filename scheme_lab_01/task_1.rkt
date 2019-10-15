#lang scheme
(define (dot_upper_abcess? y) (> y 0))
(define (sect1? a x y) (and (< (abs x) (abs a)) (> (abs y) (abs a))))
(define (sect2? a x y) (and (> (abs x) (abs a)) (< (abs y) (abs a))))
(define (sect3? a x y) (and (> (abs x) (abs a)) (> (abs y) (abs a))))
(define (calc_dist_sect3 a x y)
  (sqrt (+ (sqr (- (abs x) a)) (sqr (+ y a)))))
(define (calc_dist_sect1 a y)
  (- (abs y) (abs a)))
(define (calc_dist_sect2 a x)
  (- (abs x) (abs a)))
(define (calc_dist_if_dot_upper_abcess a x y)
    (- (sqrt (+ (sqr x) (sqr y))) a))
(define (get_dist a x y) (cond
    [(dot_upper_abcess? y) (calc_dist_if_dot_upper_abcess a x y)]
    [(sect1? a x y) (calc_dist_sect1 a y)]
    [(sect2? a x y) (calc_dist_sect2 a x)]
    [(sect3? a x y) (calc_dist_sect3 a x y)]))
(get_dist 1 -3 4)
(get_dist 1 3 4)
(get_dist 2 1 -3)
(get_dist 2 -1 -3)
(get_dist 2 3 -1)
(get_dist 2 -3 -1)
(get_dist 2 -5 -6)
(get_dist 1 2 -3)
(get_dist 1 -2 -3)