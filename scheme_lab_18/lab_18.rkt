#lang scheme
;деление списка на части за 1 проход
(define (partition x lst)
  (define (iter-lst left mid right lst)
    (if (empty? lst)
        (append (list left) (list mid) (list right))
        (let ([el (car lst)])
          (cond
            [(< el x)  (iter-lst (cons el left) mid right (cdr lst))]
            [(= el x) (iter-lst left (cons el mid) right (cdr lst))]
            [else (iter-lst left mid (cons el right) (cdr lst))]))))
  (iter-lst '() '() '() lst))
;задание 1, главная функция  
(define (quick-sort lst)
  (if (empty? lst) lst
      (let* ([head (car lst)] [parts (partition head lst)] [left (car parts)]
                              [mid (cadr parts)]
                              [right (caddr parts)])
        (append
         (quick-sort left)
         mid
         (quick-sort right)))))
;сумма цифр числа
(define (sum-of-digits n)
  (if (< n 10)
      n
      (+ (remainder n 10) (sum-of-digits (quotient n 10)))))
(define (>sum? a b)
  (> (sum-of-digits a) (sum-of-digits b)))
;задание 2, главная функция
(define (insertion-sort lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (insert (car lst) (insertion-sort (cdr lst)))]))

(define (insert n lst)
  (cond
    [(empty? lst) (list n)]
    [(>sum? n (car lst)) (cons n lst)]
    [else (cons (car lst) (insert n (cdr lst)))]))

;Задание 3
(define (find-numbers input-path output-path)
  (define in (open-input-file input-path))
  (define out (open-output-file output-path  #:exists 'replace))
  (define (writeln-file bytes)
    (displayln (bytes->string/utf-8 bytes) out))
  (begin (map writeln-file (regexp-match* #rx"[-+]?[0-9]+" in))
         (close-output-port out) (close-input-port in)) (display "done"))
;Задание 5
(define (replace-words text-path dict-path output-path)
  (define text (open-input-file text-path))
  (define dict (open-input-file dict-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define (make-replacements-lst dict)
    (define lines (port->lines dict))
    (foldl (lambda [line lst]
             (let ([str-line (string-split line)])
               (cons (cons (pregexp (string-append (car str-line) "\\b")) (cdr str-line)) lst))) '() lines))
  (define replacements (make-replacements-lst dict))
  (define lines (port->lines text))
  (define (make-replaces lines)
    (if (empty? lines)
        (begin (close-input-port text) (close-input-port dict)
               (close-output-port out))
        (begin (displayln (regexp-replaces (car lines) replacements) out)
               (make-replaces (cdr lines)))))
  (make-replaces lines))
 
      
  
  
    
    
  

      
  