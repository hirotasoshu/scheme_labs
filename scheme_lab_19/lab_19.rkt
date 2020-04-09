#lang scheme

(define (func? expr) ;проверка на то, является ли s-выражение функцией (и s-выражение ли это вообще) 
  (and (list? expr) (equal? (car expr) 'define) (or (list? (cadr expr)) (pair? (cadr expr)))))

(define (get-func-name expr) ;вытаскивает название функции из s-выражения
  (caadr expr))

(define (get-func-body expr) ;вытаскивает тело функции из s-выражения
  (cddr expr))

(define (count-functions expr) ;подсчет функций в s-выражении
  (if (empty? expr)
      0
      (if (func? (car expr))
          (+ 1 (count-functions (get-func-body (car expr))) (count-functions (cdr expr)))
          (count-functions (cdr expr)))))
          
;задача 3
(define (count-functions-in-file input-path) 
  (define in (open-input-file input-path))
  (define (next counter)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (next counter))]) ;костыль, чтобы не крашилось на #lang
      (define expr (read in))
      (if (eof-object? expr)
          (begin (close-input-port in) counter)
          (next (+ counter (count-functions (cons expr '())))))))
  (next 0))

(define (rec? expr name) ;проверяет тело функции на рекурсивность 
  (if (or (empty? expr) (not (list? expr)))
      #f
      (if (list? (car expr))
          (or (rec? (car expr) name) (rec? (cdr expr) name))
          (if (equal? (car expr) name)
              #t
              (rec? (cdr expr) name)))))

        
(define (partition pred lst) ;делит список на две части по заданному предикату
  (define (iter lst true-lst false-lst)
    (if (empty? lst)
        (append (list true-lst) (list false-lst))
        (if (pred (car lst))
            (iter (cdr lst) (cons (car lst) true-lst) false-lst)
            (iter (cdr lst) true-lst (cons (car lst) false-lst)))))
  (iter lst '() '()))

(define (find-recursive expr name) ;ищет рекурсивные и не рекурсивные функции
  (define partitioned (partition func? expr))
  (define funcs (car partitioned))
  (define exprs (cdr partitioned))
  (append
   (foldl (lambda [func res] (append res (find-recursive (get-func-body func) (get-func-name func)))) '() funcs)
   (filter-not empty? (foldl (lambda [exp result]
                               (if (rec? exp name)
                                   (cons (cons name 'recursive) result)
                                   (if name
                                       (cons (cons name 'non_recursive) result)
                                       (cons '() result)))) '() exprs))))
  
;задача 4                                                      
(define (find-recursive-in-file input-path out-nonrec-path out-rec-path) ;(самый лучший тест -- передать данный файл в качестве входного. Функция найдет рекурсивный вызов
  (define in (open-input-file input-path))                               ;функции find-recursive, который находится внутри append, который находится внутри лямбда функции,
  (define out-nonrec (open-output-file out-nonrec-path  #:exists 'replace)) ;которая находится внутри foldl, который находится внутри append)
  (define out-rec (open-output-file out-rec-path  #:exists 'replace))
  (define (next)
    (with-handlers ([exn:fail? (lambda (exn) ;костыль, чтобы не крашилось на #lang
                                 (next))])
      (define expr (read in))
      (if (eof-object? expr)
          (begin (close-input-port in) (close-output-port out-nonrec) (close-output-port out-rec))
          (begin
            (map (lambda [func]
                   (if (equal? (cdr func) 'recursive)
                       (displayln (car func) out-rec)
                       (displayln (car func) out-nonrec)))
                 (find-recursive (cons expr '()) #f))
            (next)))))
  (next))

(define (digits-in-non-decreasing-order? number) ;проверяет, в порядке ли возрастания идут цифры
  (define (check number prev-digit)
    (if (< number 10)
        (<= number prev-digit)
        (and (<= (remainder number 10) prev-digit)
             (check (quotient number 10) (remainder number 10)))))
  (check (quotient number 10) (remainder number 10)))

;задача 2
(define (find-sums-in-non-decreasing-order input-path output-path) 
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define (lines->sums lines)
      (map (lambda [line]
             (foldl + 0 (map string->number (string-split line "+")))) lines)) 
  (define result (sort (filter digits-in-non-decreasing-order? (lines->sums (port->lines in))) >))
  (begin (map (lambda [sum]
                (displayln sum out)) result) (close-output-port out) (close-input-port in)))
;задача 1
(define (formatter input-path output-path . args)
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define from-replace
    (cons "%%" (build-list (length args)
                (lambda [i]
                  (format "%~a%" (+ i 1))))))
  (define replacements
    (map (lambda [from to]
           (list (regexp from) to)) from-replace (cons "%" args)))
  (define lines (port->lines in))
  (begin
    (map (lambda [line]
           (displayln (regexp-replaces line replacements) out)) lines)
    (close-input-port in)
    (close-output-port out)))

 
  
  
  
  
