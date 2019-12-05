#lang scheme
(define (sudoku matrix)
  (define len (length (car matrix)))
  (define mainDiag (foldl (lambda [row n diag]
                            (cons (list-ref row n) diag)) null matrix
                                                          (build-list len values)))
  (define secondDiag (foldl (lambda [row n diag]
                              (cons (list-ref (reverse row) n) diag)) null (reverse matrix)
                                                                      (build-list len values)))

  (define transposed (apply map list matrix))
  (define sum (apply + (car matrix)))
  (if (foldl (lambda [row res]
               (cond
                 [(eq? res #f) #f]
                 [(= (apply + row) sum) #t]
                 [else #f]))
             #t matrix)
      (if (foldl (lambda [col res]
                   (cond
                     [(eq? res #f) #f]
                     [(= (apply + col) sum) #t]
                     [else #f]))
                 #t transposed)
          (if (= (apply + mainDiag) sum)
              (if (= (apply + secondDiag) sum)
                  #t
                  #f)
              #f)
          #f
          )
      #f
      )
  )