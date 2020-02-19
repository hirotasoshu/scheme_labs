#lang scheme
(define (coef-lst->coef-deg-lst pow coef-lst)
  (define pow-lst (build-list pow (lambda [x]
                                         (- pow (+ x 1)))))
  (map (lambda [coef degree]
         (cons coef degree)) coef-lst pow-lst))

(define (mult-coef-deg coef-deg1 coef-deg2)
  (define new-coef (* (car coef-deg1) (car coef-deg2)))
  (define new-deg (+ (cdr coef-deg1) (cdr coef-deg2)))
  (cons new-coef new-deg))
  
(define (polymulty coef-lst1 coef-lst2)
  (define pow1 (length coef-lst1))
  (define pow2 (length coef-lst2))
  (define coef-deg-lst1 (coef-lst->coef-deg-lst pow1 coef-lst1))
  (define coef-deg-lst2 (coef-lst->coef-deg-lst pow2 coef-lst2))
  (define max-deg (- (+ pow1 pow2) 2))
  (define (iter-fpoly coef-deg-lst1 result)
    (define (iter-spoly coef-deg coef-deg-lst2 result)
      (if (empty? coef-deg-lst2)
          result
          (iter-spoly coef-deg (cdr coef-deg-lst2)
                      (cons (mult-coef-deg coef-deg (car coef-deg-lst2)) result))))
    (if (empty? coef-deg-lst1)
        result
        (let ([new-result (iter-spoly (car coef-deg-lst1) coef-deg-lst2 result)])
          (iter-fpoly (cdr coef-deg-lst1) new-result))))
  (define result (iter-fpoly coef-deg-lst1 '()))
  (define (bringing-similar poly deg)
    (define coef
      (foldl (lambda [el res]
               (+ (car el) res)) 0 (filter (lambda [el]
                                             (= (cdr el) deg)) result)))
    (if (= deg max-deg)
        (cons coef poly)
        (bringing-similar (cons coef poly) (+ deg 1))))
  (bringing-similar '() 0))

(define (several-polymulty . args)
  (foldl (lambda [el res]
           (polymulty el res)) (car args) (cdr args)))
  
          
      
      
  
  
  

  