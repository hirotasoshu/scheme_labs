#lang scheme
(define (get-max-column-elements matrix)
  (apply map max matrix))
(define (get-min-row-elem row)
  (apply min row))
(define (get-row-seddle-points row max-col-elems)
  (define min-row-elem (get-min-row-elem row))
  (foldl (lambda [elem max-col-elem result]
           (if (= elem max-col-elem min-row-elem)
               (cons elem result)
               result)) '() row max-col-elems))
(define (seddle-points matrix)
  (define max-col-elems (get-max-column-elements matrix))
  (define result (flatten (map (lambda [row]
         (get-row-seddle-points row max-col-elems)) matrix)))
  (if (empty? result)
      #f
      result))
 
  
  
  
  
  
  