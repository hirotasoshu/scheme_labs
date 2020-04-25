#lang scheme
;1 задача (так как граф неориентированный, достаточно посчитать количество нулевых строк)
(define (count-isolated-vertex input-path)
  (define in (open-input-file input-path))
  (define lines (port->lines in))
  (begin (close-input-port in)
         (foldl (lambda [line counter]
                  (if (andmap (lambda [x]
                                (equal? x "0")) (string-split line))
                      (+ counter 1)
                      counter)) 0 lines)))

(define (string-head-num str)
  (string->number (car (string-split str)))) ;вспомогательная функция, которая возвращает первое число из строки 

;2 задача
(define (complete-undirected-graph? input-path) ;так как граф неориентированный и без петель, то у каждой вершины должно быть n-1 смежных с ней
  (define in (open-input-file input-path))
  (define lines (port->lines in))
  (define n (string-head-num (car lines)))
  (define n-1 (- n 1))
  (begin (close-input-port in)
         (andmap (lambda [line]
                   (= (string-head-num line) n-1)) (cdr lines))))

;4 задача
(define (adjacency-matrix->edge-list input-path output-path)
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define (iter-line i j line)
    (cond
      [(empty? line) (let ([new_line (read-line in)])
                       (if (eof-object? new_line)
                           (begin (close-output-port out) (close-input-port in))
                           (iter-line (+ i 1) (+ i 1) (drop (string-split new_line) (+ i 1)))))] ;начинаем с i+1 элемента строки, а не с i+2,
      [(equal? (car line) "1") (begin (displayln (format "~a ~a" i j) out) ;чтобы не пропустить возможную петлю
                                      (iter-line i (+ j 1) (cdr line)))]
      [else (iter-line i (+ j 1) (cdr line))]))
  (iter-line 0 0 (string-split (read-line in))))

;3 задача
(define (adjacency-lists->edge-list input-path output-path)
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define (iter-lines i line)
    (cond
      [(empty? line) (let ([new_line (read-line in)])
                       (if (eof-object? new_line)
                           (begin (close-output-port out) (close-input-port in))
                           (iter-lines (+ i 1) (cdr (string-split new_line)))))]
      [(<= i (string->number (car line))) ;<= чтобы не пропустить петлю
       (begin (displayln (format "~a ~a" i (string->number (car line))) out)
              (iter-lines i (cdr line)))]
      [else (iter-lines i (cdr line))]))
  (read-line in) ;чтобы скипуть первую строку, в которой указано количество вершин графа
  (iter-lines 0 (cdr (string-split (read-line in)))))

;5 задача
(define (adjacency-lists->adjacency-matrix input-path output-path)
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define n (string-head-num (read-line in)))
  (define (iter-adj-lists lst)
    (if (eof-object? lst)
        (begin (close-input-port in) (close-output-port out))
        (let ([current_lst (map string->number (cdr (string-split lst)))])
          (begin (build-list n (lambda [x]
                                 (if (member x current_lst)
                                     (begin (display "1 " out))
                                     (display "0 " out))))
                 (display #\newline out))
                 (iter-adj-lists (read-line in)))))
  (iter-adj-lists (read-line in)))
                               
          
  
       




              
              
  
  

  
    


      