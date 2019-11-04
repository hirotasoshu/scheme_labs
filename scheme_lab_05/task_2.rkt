#lang scheme
(define (get-mean lst)
  (define (iter lst sum count)
        (if (empty? lst)
            (if (= count 0)
                #f
                (/ sum count))
            (iter (cdr lst) (+ sum (car lst)) (+ count 1))))
  (iter lst 0 0))
            
(define (find-nearest-to-mean lst)
  (define mean (get-mean lst))
  (define (iter lst delta answer num i)
    (if (eq? mean #f) #f
    (let ([curr_delta (if (empty? lst) #f (abs (- (car lst) mean)))])
    (cond
      [(eq? curr_delta #f) (reverse answer)]
      [(< curr_delta delta) 
       (iter (cdr lst) curr_delta (cons i (cons (car lst) '())) (car lst) (+ i 1))]
      [(= curr_delta delta) 
       (if (= (car lst) num)
           (iter (cdr lst) delta (cons i answer) num (+ i 1))
           (if (< (car lst) num)
               (iter (cdr lst) delta (cons i (cons (car lst) '())) (car lst) (+ i 1))
               (iter (cdr lst) delta answer num (+ i 1))))]
        [else (iter (cdr lst) delta answer num (+ i 1))]))))
  (iter lst +inf.0 '() +inf.0 1))
(find-nearest-to-mean '(5 5 5))
(find-nearest-to-mean '(10 0 5))

      
