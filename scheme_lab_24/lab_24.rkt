#lang scheme
#|
Задача 1
Формат входных данных: список чисел
Формат выходных данных: перестановка с максимальной суммой наибольших общих делителей
пар последовательно идущих элементов списка.
Пример входных данных:
(0 3 2 6 5)
Пример выходных данных:
(2 5 0 6 3)
|#  
(define (max-gcd-sum lst) ;главная функция
  (define all-permutations (number-permutations lst))
  (cdr (foldl (lambda [permutation answer]
                (let ([current-sum (gcd-sum permutation)])
                  (if (> current-sum (car answer))
                      (cons current-sum permutation)
                      answer))) '(0) all-permutations)))

;вспомогательная функция
(define (gcd-sum lst)
  (define (iter lst sum)
    (if (empty? (cdr lst))
        sum
        (iter (cdr lst) (+ sum (gcd (car lst) (cadr lst))))))
  (iter lst 0))

;генерируем все перестановки чисел
(define (number-permutations lst)
  (define (iter lst tail)
    (if (empty? lst)
        (list tail)
        (append-map (lambda [x]
                      (iter (remove x lst =) (cons x tail))) lst))) ;передаем в remove = как функцию сравнения, ибо у нас просто числа, equal? не нужен
  (iter lst '()))

#|
Задача 2
Формат входных данных: список точек на плоскости в виде пар
Формат вывода: собственные подмножества с максимальной суммой расстояний до центра масс подмножества в виде списка списков точек
Пример ввода: ((5 . 5) (-5 . 5) (-5 . -5) (5 . -5))
Пример вывода:
 (((-5 . 5) (-5 . -5) (5 . -5))
 ((5 . 5) (-5 . -5) (5 . -5))
 ((5 . 5) (-5 . 5) (5 . -5))
 ((5 . 5) (-5 . 5) (-5 . -5)))
|#


(define (distances-to-center-sum points) ;сумма квадратов расстояний до центра множества точек
  (define center (cons (/ (apply + (map car points)) (length points)) (/ (apply + (map cdr points)) (length points))))
  (define (distance-to-center point)
    (+ (sqr (- (car point) (car center))) (sqr (- (cdr point) (cdr center))))) ;нет смысла извлекать корень, нам важны сами подмножества, а не точная сумма расстояний
  (foldl (lambda [point sum]
           (+ sum (distance-to-center point))) 0 points))
           
(define (powerset lst) ;булеан множества
  (if (empty? lst)
      '(())
      (append-map (lambda [x]
                    (list x (cons (car lst) x)))
                  (powerset (cdr lst)))))

(define (find-subsets points) ;главная функция
  (define proper-subsets (cdr (drop-right (powerset points) 1))) ;все собственные подмножества
  (define (iter-subsets subsets max-sum result)
    (if (empty? subsets)
        result
        (let* ([current-subset (car subsets)] [current-sum (distances-to-center-sum current-subset)])
          (if (> current-sum max-sum)
              (iter-subsets (cdr subsets) current-sum (list current-subset))
              (if (= current-sum max-sum)
                  (iter-subsets (cdr subsets) max-sum (cons current-subset result))
                  (iter-subsets (cdr subsets) max-sum result))))))
  (iter-subsets proper-subsets 0 '()))

#|
Задача 3
Формат ввода: список чисел и число
Формат вывода: цепочка чисел максимальной длины, начинающаяся заданным на входе числом и продолжающаюся числами из списка,
такая, что для каждой последовательной пары элементов этой цепочки них наибольший общий делитель был строго больше единицы.
Пример ввода: (1 2 4 6 3 5 7 8) 12
Пример вывода: (12 3 6 2 4 8)
|#
(define (list-with-max-length lst1 lst2) ;вспомогательная функция
  (if (> (length lst1) (length lst2))
      lst1
      lst2))

(define (gen-max-chain lst n) ;главная функция
  (define (max-chain lst current-chain result) ;ищем наибольшую подпоследовательность для какой-то перестановки
    (if (empty? lst)
        (reverse (list-with-max-length current-chain result))
        (if (= (gcd (car current-chain) (car lst)) 1)
            (if (= (length current-chain) 1)
                (max-chain (cdr lst) current-chain result)
                (max-chain lst (list n) (list-with-max-length current-chain result)))
            (max-chain (cdr lst) (cons (car lst) current-chain) result))))
  (argmax length (foldl (lambda [permutation chains]
                          (cons (max-chain permutation (list n) (list n)) chains)) '() (number-permutations lst)))) ;находим максимальную цепочку из наибольших подпоследовательностей всех перестановок
        
            