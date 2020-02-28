#lang scheme
(define (strings-list->string lst str)
  (if (empty? lst)
      str
      (strings-list->string (cdr lst) (string-append str (car lst)))))
(define (root? directory)
  (eq? #\/ (last (string->list directory))))
(define (get-path lst)
  (define (iter-directories lst root prev-directory prev-prev-directory path-lst)
    (if (empty? lst)
        (if root
            (strings-list->string (cons root (add-between (reverse path-lst) "/")) "")
            #f)
        (let ([directory (car lst)])
          (if root
              (if (eq? directory prev-prev-directory)
                  (iter-directories (cdr lst) root directory prev-directory (cdr path-lst))
                  (iter-directories (cdr lst) root directory prev-directory (cons directory path-lst)))
              (if (root? directory)
                  (iter-directories (cdr lst) directory #f #f '())
                  (iter-directories (cdr lst) #f #f #f '()))))))
  (iter-directories lst #f #f #f '()))
 
