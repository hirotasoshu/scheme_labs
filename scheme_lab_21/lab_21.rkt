#lang scheme
;задача 1
;в каждом списке смежности находятся только вершины, смежные данным
;возвращаются списки смежности отсортированного графа
;если нужно вернуть список вершин в отсортированном порядке, нужно убрать последний foldr и вернуть stack
(define (top-sort adj-lists)
  (define (dfs vertex stack)
    (cons vertex (foldl (lambda [current-vertex stack]
                          (if (member current-vertex stack)
                              stack
                              (dfs current-vertex stack))) stack (list-ref adj-lists vertex))))
  (define vertexes-list (build-list (length adj-lists) values))
  (define stack (foldl (lambda [current-vertex stack]
                         (if (member current-vertex stack)
                             stack
                             (dfs current-vertex stack))) '() vertexes-list))
  (define (index n k lst)
    (if (= (car lst) n)
        k
        (index n (+ k 1) (cdr lst))))
  (foldr (lambda [old-vertex res]
           (cons (map (lambda [old-vertex-num] (index old-vertex-num 0 stack)) (list-ref adj-lists old-vertex)) res)) '() stack))


  
;задача 2
;вспомогательные функции
(define (normalize x y)
  (if (> x y)
      (- x 1)
      x))

(define (cadr/cdr maybe-list)
  (if (list? maybe-list)
      (cadr maybe-list)
      (cdr maybe-list)))

;главная функция
(define (remove-vertexes edges-list vertexes-to-remove)
  (foldl (lambda [vertex-to-remove new-vertex rez]
           (foldl (lambda [edge res]
                    (let ([first (car edge)] [second (cadr/cdr edge)] [diff (- vertex-to-remove new-vertex)])
                      (if (or (= first diff) (= second diff))
                          res
                          (cons
                           (cons (normalize first diff)
                                 (normalize second diff)) res)))) '() rez))
         edges-list (sort vertexes-to-remove <) (build-list (length vertexes-to-remove) values)))

;задача 3
;в каждом списке смежности находятся только вершины, смежные данным
(define (graph-addition adj-lists)
  (define vertexes (build-list (length adj-lists) values))
  (foldr (lambda [adj-list vertex res]
           (cons (remove vertex (remove* adj-list vertexes)) res)) '() adj-lists vertexes))

;задача 4
;в каждом списке смежности находятся только вершины, смежные данным
(define (connected-components adj-lists)
  (define (dfs vertex stack unused-vertexes)
    (foldl (lambda [current-vertex stack]
             (if (or (member current-vertex stack) (not (member current-vertex unused-vertexes)))
                 stack
                 (dfs current-vertex (cons current-vertex stack) unused-vertexes))) stack (list-ref adj-lists vertex)))
  (define vertexes (build-list (length adj-lists) values))
  (define (find-components components unused-vertexes vertexes)
    (cond
      [(empty? vertexes) components]
      [(member (car vertexes) unused-vertexes)
       (let* ([maybe-new-component (dfs (car vertexes) '() unused-vertexes)]
              [new-component (if (empty? maybe-new-component) (list (car vertexes)) maybe-new-component)])
         (find-components (cons new-component components) (remove* new-component unused-vertexes) (cdr vertexes)))]
      [else (find-components components unused-vertexes (cdr vertexes))]))
  (find-components '() vertexes vertexes))

;задача 5
;в каждом списке смежности находятся только вершины, смежные данным
;так как граф неориентированный, поиск в ширину позволит найти кратчайший путь от точки а до точки б
(define (BFS a b G)
  (define (iter prosm queue)
    (if (> (list-ref prosm a) 0) (list-ref prosm (car queue))
        (if (empty? queue) #f
            (let* ((pos (car queue))

                   (next (filter (lambda (x) (= 0 (list-ref prosm x)))
                                 (list-ref G pos)))
                   (step (list-ref prosm pos)))
              (if (equal? next #f) (iter prosm (cdr queue))
                  (iter (map (lambda (i)
                               (if (= 0 (list-ref prosm i))
                                   (if (equal? (member i next) #f) 0

                                       (+ step 1))
                                   (list-ref prosm i)))
                             (build-list (length G) values))
                        (append (cdr queue) next)))))))
  (iter (build-list (length G) (lambda (i) (if (= i b) 1 0))) (list b)))
;функция, возвращающая матрицу расстояний
(define (make-dist-matrix adj-lists)
  (define vertexes (build-list (length adj-lists) values))
  (foldr (lambda [vertex-from matrix]
           (cons
            (foldr (lambda [vertex-to row]
                          (let ([dist (BFS vertex-from vertex-to adj-lists)])
                            (if dist
                                (cons dist row)
                                (cons +inf.0 row)))) '() vertexes)
            matrix)) '() vertexes))
;главная функция
(define (find-graph-center adj-lists)
  (define dist-matrix (make-dist-matrix adj-lists))
  (define eccentricity-list (apply map max dist-matrix))
  (define (iter lst center i min)
    (cond
      [(empty? lst) center]
      [(< (car lst) min)
       (iter (cdr lst) (list i) (+ i 1) (car lst))]
      [(= (car lst) min)
       (iter (cdr lst) (cons i center) (+ i 1) min)]
      [else (iter (cdr lst) center (+ i 1) min)]))
  (iter eccentricity-list '() 0 +inf.0))
                                
  
  

      
  
