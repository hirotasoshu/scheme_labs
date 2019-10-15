#lang scheme
(define (format? hours) (> hours 12))
(define (calculate_angle_between_arrows hours minutes)
    ( - (* (+ hours (/ minutes 60)) 30) (* 6 minutes)))
(define (get_angle_between_arrows hours minutes) (cond
    [(format? hours) (calculate_angle_between_arrows (- hours 12) minutes)]
    [else (calculate_angle_between_arrows hours minutes)]))
(get_angle_between_arrows 3 15)
(get_angle_between_arrows 15 15)

