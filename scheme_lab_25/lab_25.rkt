#lang scheme
#|
Задача 1
Входные данные: последовательность в виде списка
Выходные данные: разряженная подпоследовательность с максимальной суммой
Пример входа: (4 8 3 2 10)
Пример выхода: (8 10)
|#
(define (sparse-sequence sequence)
  (define (iter-sequence sequence sum trace) ;вся динамика заключается в использовании списка максимальных сумм, где sum[i] = max(sum[i-2]+sequence[i], sum[i-1])
    (if (empty? sequence) ;где sum[i-2]+sequence[i] -- максимальная сумма включая текущий элемент, sum[i-1] -- максимальная сумма не включая текущий элемент
        (reverse (car trace)) ;trace[i] -- способ максимизации суммы для sum[i] (искомая подпоследовательность для первых i элементов)
        (let* ([elem (car sequence)][inclusive-sum (+ elem (cadr sum))]
                                    [non-inclusive-sum (car sum)]) 
          (if (> inclusive-sum non-inclusive-sum)
              (iter-sequence (cdr sequence) (cons inclusive-sum sum) (cons (cons elem (cadr trace)) trace))
              (iter-sequence (cdr sequence) (cons non-inclusive-sum sum) (cons (car trace) trace))))))
  (iter-sequence sequence '(0 0) '(()()))) ;для удобства инициализируем список сумм изначально с двумя нулями

#|
Задача 2
Входные данные: расписание выдачи контейнеров в виде списка
Выходные данные: распределение поступающих контейнеров (на нулевом месте стоит необходимое количество складов)
Пример входных данных: (3 1 4 2 5)
Пример выходных данных: (2 1 1 2 2 2)
|#

#|
По-сути, задача сводится к тому, чтобы найти наименьшее число возрастаующих подпоследовательностей, покрывающих всю последовательность
Это можно реализовать с помощью пасьянсной (или, как еще говорят, терпеливой) сортировки (англ patience sort)
|#

;главная функция
(define (containers schedule)
  (define containers-num (length schedule))
  (define splited-containers (split schedule))
  (define splits-num (length splited-containers))
  (define (get-answer i splits result) ;восстанавливаем ответ
    (if (= i 0)
        (cons splits-num result)
        (get-answer (- i 1) (cdr splits)
                    (foldl (lambda [elem result] (list-set result (- elem 1) i)) result (car splits)))))
  (get-answer splits-num splited-containers (build-list containers-num values))) 
                 
  

(define (split sequence) ;подфункция, жадно разбивающая ящики на склады. Работает за O(n log n)
  (define (bsearch-piles x len piles) ;бинпоиск
    (let aux ([lo 0]
              [hi (- len 1)])
      (if (> lo hi)
          lo
          (let ([mid (quotient (+ lo hi) 2)])
          (if (> (car (list-ref piles mid)) x)
              (aux (+ mid 1) hi)
              (aux lo (- mid 1)))))))
  (define (make-piles sequence len piles) ;пасьянсная (терпеливая) сортировка, только возвращаем вместо отсортированного списка сами стопки
    (if (empty? sequence) 
        piles
        (let* ([x (car sequence)]
               [index (bsearch-piles x len piles)])
          (if (= index len)
              (make-piles (cdr sequence) (+ len 1) (append piles (list (list x))))
              (make-piles (cdr sequence) len (list-update piles index
                                                                (lambda [pile] (cons x pile))))))))
  (make-piles sequence 0 '()))


  
#|
Задача 3
Входные данные: n (кол-во служащих), список из n списков, каждый k-ый список содержит в себе список подчиненных k-го служащего (без нулей на конце,
если нет подчиненных -- пустой список!!), ID Тани
Выходные данные: минимальное время, за которое вся компания узнает о гениальном методе Тани
Пример входных данных 1: 9 ((2 8) (3 4) () (5 6) () () () (9) (7)) 1
Пример выходных данных 1: 4
Пример входных данных 2: 10 ((2 3) (4 5 7) (6 9) () () (8 10) () () () ()) 2
Пример выходных данных 2: 5
|#

  #|
Жадник; Используя дфс, рекурсивно вычисляем оптимальное время из узла Тани до каждой
из её коллег, и сортируем эти узлы в зависимости от затраченного времени.
Оптимальным временем для Тани является время, необходимое для распространения сообщения,
когда Таня звонит коллегам (узлам) по порядку уменьшения времени. Это делается рекурсивно.
Сложность алгоритма (без учета перевода ориентированного графа в неориентированный) O(n log n)
|#

  (define (classmates n adj-list id) ;главная функция, обертка над dfs и превращение ориентированного графа в неориентированный
    (define (directed->undirected adj-list) ;выглядит неадекватно, но у меня уже голова не очень соображает, чтобы что-то лучше делать
      (foldl (lambda [node result]
               (foldl (lambda [current-node modified]
                        (list-update modified (- current-node 1)
                                     (lambda [x]
                                       (if (member node x)
                                           x
                                           (cons node x))))) result (list-ref adj-list (- node 1)))) adj-list (build-list n add1)))
    (dfs id #f (directed->undirected adj-list)))
    
    

  (define (dfs node parent adj-list) ;вся магия происходит здесь
    (define times (sort (foldl (lambda [current-node result]
                                 (if (equal? current-node parent) ;в parent на первой итерации лежит #f, поэтому equal?
                                     result
                                     (cons (dfs current-node node adj-list) result))) '() (list-ref adj-list (- node 1))) >))
    (define (iter-times times i max-t) ;i -- время, прошедшее с момента получения звонка
      (if (empty? times)
          max-t
          (if (> (+ (car times) i) max-t) 
              (iter-times (cdr times) (+ i 1) (+ (car times) i))
              (iter-times (cdr times) (+ i 1) max-t))))
    (iter-times times 1 0))

  
  
  
  
  