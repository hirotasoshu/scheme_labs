#lang scheme

;первая задача
#|
Формат ввода и вывода:
(n (..) (...)), где n - общее кол-во вершин в графе, а в каждом списке находится вершина, смежная данной
Пример входных данных:
(3 (1 2) (2) ())
Пример выходных данных:
(3 () (0) (1 0))
|#

(define (transpose-graph graph)
  (define n (car graph))
  (define arcs-list (cdr graph))
  (define (transpose current-node arcs-list result)
    (if (< current-node n)
        (transpose (+ current-node 1)
                   (cdr arcs-list)
                   (foldl (lambda [node res]
                            (list-update res node
                                         (lambda [arc]
                                           (cons current-node arc)))) result (car arcs-list)))
        result))
  (cons n (transpose 0 arcs-list (make-list n '()))))

;вторая задача
#|
Формат ввода:
(n (...)...(...)), где n - общее кол-во вершин в графе, а в каждом списке находится вершины, смежные с данной
Формат вывода:
((...)(...)...(...)), где в каждом списке находятся тройки точек
Пример входных данных 1:
(4 (1 3) (0 2 3) (1 3) (0 1 2))
Пример выходных данных 1:
((0 1 3) (1 2 3))
Пример входных данных 2:
(3 (1) (0) ())
Пример выходных данных 2:
#f
|#

(define (find-complete-subgraphs graph)
  (define n (car graph))
  (define adj-lists (cdr graph))
  (define (iter-a a result)
    (define (iter-b b result-temp1)
      (define (iter-c c result-temp2)
        (cond
          [(= c n) (iter-b (+ b 1) (append result-temp1 result-temp2))]
          [{and (member b (list-ref adj-lists a))
                (member c (list-ref adj-lists a))
                (member c (list-ref adj-lists b))}
           (iter-c (+ c 1) (cons (list a b c) result-temp2))]
          [else (iter-c (+ c 1) result-temp2)]))
      (if (= b (- n 1))
          (iter-a (+ a 1)  (append result result-temp1))
          (iter-c (+ b 1) '())))
    (if (= a (- n 2))
        result
        (iter-b (+ a 1) '())))
  (let ([res (iter-a 0 '())])
    (if (empty? res)
        #f
        res)))

;3 задача
#|
Формат ввода: (узел_дерева левое_поддерево правое_поддерево)
Формат вывода:
(n (..) (...)), где n - общее кол-во вершин в графе, а в каждом списке находится вершина, смежная данной
Пример входных данных:
(0 (1 (3 () ()) (4 (5 () (6 () ())) ())) (2 () ()))
Пример выходных данных:
(7 (1 2) (0 3 4) (0) (1) (1 5) (4 6) (5))
|#
(define left-branch cadr)
(define right-branch caddr) 
(define data car)
(define (caddaar lst) (car (cddaar lst)))
(define (caadaar lst) (car (cadaar lst)))
(define (tree->graph tree)
  (define (iter-nodes buff-tree prev)
    (define (get-nodes tree)
      (cons (list (left-branch tree) (right-branch tree) prev) (car tree)))
    (if (empty? buff-tree)
        '()
        (append (list (get-nodes buff-tree))
                (iter-nodes (left-branch buff-tree) (car buff-tree))
                (iter-nodes (right-branch buff-tree) (car buff-tree)))))
  (define obfuscated-graph (sort (iter-nodes tree '()) < #:key cdr))
  (define (deobfuscate-graph n obfuscated-graph deobfuscated-graph)
    (if (empty? obfuscated-graph)
        (append (list n) deobfuscated-graph)
        (deobfuscate-graph (+ n 1) (cdr obfuscated-graph)
                           (append deobfuscated-graph
                                   (list (flatten (append (if (empty? (cddaar obfuscated-graph)) '() (list (caddaar obfuscated-graph)))
                                                          (if (empty? (caaar obfuscated-graph)) '() (list (caaaar obfuscated-graph)))
                                                          (if (empty? (cadaar obfuscated-graph)) '() (list (caadaar obfuscated-graph)))
                                                          )))))))
  (deobfuscate-graph 0 obfuscated-graph '()))

        
;4 задача
#|
Формат ввода:
(n (..) (...)), где n - общее кол-во вершин в графе, а в каждом списке находится вершина, смежная данной
Формат вывода:
#t/#f
Пример ввода 1:
(7 (1 2) (0 3 4) (0) (1) (1 5) (4 6) (5))
Пример вывода 1:
(0 (1 (3 () ()) (4 (5 (6 () ()) ()) ())) (2 () ()))
Пример ввода 2:
(7 (1 3 5) (0 2) (1) (0 4 5) (3) (3) ())
Пример вывода 2:
#f
|#
(define (graph->tree? graph)
  (define n (car graph))
  (define adj-lists (cdr graph))
 (if (bin-tree? adj-lists n)
     (make-tree adj-lists)
     #f))

(define (make-tree adj-lists)
  (define root (index-where adj-lists (lambda [adj] (<= (length adj) 2))))
  (define (iter-tree node prev-node)
    (define adj-nodes (remove prev-node (list-ref adj-lists node)))
    (if (empty? adj-nodes)
        (list node '() '())
        (list
         node
         (iter-tree (car adj-nodes) node)
         (if (empty? (cdr adj-nodes))
                     '()
                     (iter-tree (cadr adj-nodes) node)))))
  (iter-tree root #f))

;проверка на ацикличность, связность и степени вершин
(define (bin-tree? adj-lists n)
  (define (dfs vertex prev-vertex stack unused-vertexes)
    (foldl (lambda [current-vertex stack]
             (if stack
                 (if (or (= current-vertex prev-vertex) (not (member current-vertex unused-vertexes)))
                     stack
                     (if (member current-vertex stack)
                         #f
                         (dfs current-vertex vertex (cons current-vertex stack) unused-vertexes)))
                 #f))
           stack (list-ref adj-lists vertex)))
  (define vertexes (build-list n values))
  (define (acyclic? unused-vertexes vertexes)
    (cond
      [(empty? vertexes) #t]
      [(member (car vertexes) unused-vertexes)
       (let* ([maybe-new-component (dfs (car vertexes) (car vertexes) '() unused-vertexes)]
              [new-component (if (empty? maybe-new-component) (list (car vertexes)) maybe-new-component)])
         (if new-component
             (acyclic? (remove* new-component unused-vertexes) (cdr vertexes))
             #f))]
      [else (acyclic? unused-vertexes (cdr vertexes))]))
  (define (check-degrees adj-lists)
    (andmap (lambda [lst]
              (<= (length lst) 3)) adj-lists))
  (define (check-edges adj-lists n)
    (= (- (* 2 n) 2)
       (foldl (lambda [lst counter]
                (+ counter (length lst))) 0 adj-lists)))
  (and (acyclic? vertexes vertexes) (check-degrees adj-lists) (check-edges adj-lists n)))
  
  



                 
  
  
