#lang scheme

;задача 4

#|
Вспомогательная функция, которая проверяет на одинаковость соотношение сторон описаных прямоугольников вокруг заданных многоугольников.
Если они совпадают, то функция возвращает отношение соответствующих сторон прямоугольников, которое является коэфицентом масштабирования.
|#
(define (scaling-factor polygon1 polygon2)
  (define (compute-aspect-ratio polygon)
    (define xs (map car polygon))
    (define ys (map cdr polygon))
    (define deltax  (- (apply max xs) (apply min xs))) ;O(2n)=O(n) < O(n * log n), поэтому используем min-max вместо сортировки 
    (define deltay (- (apply max ys) (apply min ys)))
    (cons deltax deltay))
  (define s1 (compute-aspect-ratio polygon1))
  (define s2 (compute-aspect-ratio polygon2))
  (if (= (/ (car s1) (cdr s1)) (/ (car s2) (cdr s2)))
      (/ (car s2) (car s1))
      #f))

;Вспомогательная функция, которая находит левый верхний угол многоугольника.
(define (top-left polygon)
  (define (iter polygon top-left-vertex)
    (if (empty? polygon)
        top-left-vertex
        (let ([vertex (car polygon)])
          (cond
            [(< (car vertex) (car top-left-vertex)) (iter (cdr polygon) vertex)]
            [(> (car vertex) (car top-left-vertex))  (iter (cdr polygon) top-left-vertex)]
            [(> (cdr vertex) (cdr top-left-vertex)) (iter (cdr polygon) vertex)]
            [else (iter (cdr polygon) top-left-vertex)]))))
  (iter (cdr polygon) (car polygon)))

(define (~= a b)
  (<= (abs (- a b)) 0.000001)) ;были небольшие проблемы с точностью)

;вспомогательная фунция
(define (contain-vertex? vertex polygon) ;причина существования этой функции: > (member '(1.0 . 2) '((1 . 2))) >#f
  (ormap (lambda [other-vertex] ;связано с работой функции equal? : >(equal? 5 5) >#t >(equal? 5 5.0) > #f
           (and (~= (car vertex) (car other-vertex)) (~= (cdr vertex) (cdr other-vertex)))) polygon))
#|
Главная функция.
Формат ввода: ((x1 . y1) (x2 . y2) ... (xn . yn)) ((X1 . Y1) (X2 . Y2) ... (XN . YN)).
Многоугольники задаются именно списком ПАР(!) координат, а не списком списков.
Формат вывода:
#t/#f
Пример ввода 1:
'((2 . 3) (1 . 1) (3 . 1)) '((5 . 1) (9 . 1) (7 . 5)))
Пример вывода 1:
#t
Пример ввода 2:
'((2 . 3) (1 . 1) (3 . 2)) '((5 . 1) (9 . 1) (7 . 5)))
Пример вывода 2:
#f
|#

(define (transformed-polygon? polygon1 polygon2)
  (define s (scaling-factor polygon1 polygon2))
  (if s
      (let* (
      [top-left1 (top-left polygon1)]
      [top-left2 (top-left polygon2)]
      [xtl (car top-left1)]
      [ytl (cdr top-left1)]
      [Xtl (car top-left2)]
      [Ytl (cdr top-left2)])
        (define (translation vertex) ;формула параллельного переноса с масштабированием
          (define X (+ (* s (- (car vertex) xtl)) Xtl))
          (define Y (+ (* s (- (cdr vertex) ytl)) Ytl))
          (cons X Y))
        (andmap (lambda [vertex]
                  (contain-vertex? (translation vertex) polygon2)) polygon1))
  #f))

;задача 1
#|
Формат ввода: n
Формат вывода: список координат точек правильного n-угольника в виде пар
Пример ввода: 4
Пример вывода:
((1 . 0) (0.0 . 1.0) (-1.0 . 0.0) (-0.0 . -1.0)) 
|#
(define (build-polygon n) ;координаты вычисляются по формуле: x = x0 + Rcos(phi0 + 2*pi*i/n)
  (build-list n (lambda [i] ;  y = y0 + Rsin(phi0 + 2*pi*i/n)
                (let ([angle (/ (* 2 pi i) n)]) ;Я выбрал x0=y0=0, R=1, phi0=0
                  (cons (fcos angle) (fsin angle))))))

(define (fcos angle) ;округление косинуса до 5 знаков после запятой
  (/ (round (* (cos angle) 100000)) 100000))

(define (fsin angle) ;округление синуса до 5 знаков после запятой
  (/ (round (* (sin angle) 100000)) 100000))
        
;задача 5    
#|
Формат ввода: список координат точек треугольника в виде пар
Формат вывода: пара координат центра описанной окружности вокруг треугольника
Пример ввода: ((5 . 4) (2 . 1) (-3 . 7))
Пример вывода:
(19/22 . 5 3/22)
|#

(define (center triangle)
  (define x1 (caar triangle)) ;все выводится составлением системы уравнений из уравнения окружности
  (define y1 (cdar triangle))
  (define x2 (caadr triangle))
  (define y2 (cdadr triangle))
  (define x3 (caaddr triangle))
  (define y3 (cdaddr triangle))
  (define x12 (- x1 x2))
  (define x23 (- x2 x3))
  (define x31 (- x3 x1))
  (define y12 (- y1 y2))
  (define y23 (- y2 y3))
  (define y31 (- y3 y1))
  (define z1 (+ (* x1 x1) (* y1 y1)))
  (define z2 (+ (* x2 x2) (* y2 y2)))
  (define z3 (+ (* x3 x3) (* y3 y3)))
  (define z (- (* x12 y31) (* y12 x31)))
  (define center-x (- (/ (+ (* y12 z3) (* y23 z1) (* y31 z2)) (* 2 z))))
  (define center-y (/ (+ (* x12 z3) (* x23 z1) (* x31 z2)) (* 2 z)))
  (cons center-x center-y))

;задача 3
#|
Формат ввода: список координат точек в виде списков
Формат вывода: координаты параллелотопной оболочки
Пример ввода 1:
((3 7) (-2 4) (-1 3) (2 4) (5 2) (3 -5))
Пример вывода 1:
((-2 -5) (-2 7) (5 -5) (5 7))
Пример ввода 2:
((1 3 7) (2 -2 4) (5 -1 3) (6 2 4) (-3 5 2) (4 3 -5))
Пример вывода 2:
((-3 -2 -5) (-3 -2 7) (-3 5 -5) (-3 5 7) (6 -2 -5) (6 -2 7) (6 5 -5) (6 5 7))
|#
(define (parallelotope-shell points) 
  (define mins (apply map min points)) ; список из минимальных координат каждой оси
  (define maxs (apply map max points)) ; список из максимальных координат каждой оси
  (define min-max (map list mins maxs)) ;каждый список состоит из минимальной и максимальной координаты по какой-то из осей
  (apply cartesian-product min-max)) ;очевидно, что декартово произведения множеств, содержащих минимальные и максимальные координаты осей, даст координаты минимального параллелотопа

;задача 2
#|
Формат ввода: список координат многоугольника в виде пар
Формат вывода: список списков пар коордиант треугольнов
Пример ввода: '((0 . 1) (-1 . 0) (0 . -1) (1 .  0))
Пример вывода: (((0 . 1) (0 . -1) (1 . 0)) ((0 . 1) (-1 . 0) (0 . -1)))
|#
(define (polygon-triangulation polygon)
  (define start-point (car polygon))
  (define (iter current-point next-point polygon triangles)
    (if (empty? (cdr polygon))
       (cons (list start-point current-point next-point) triangles)
       (iter next-point (cadr polygon) (cdr polygon) (cons (list start-point current-point next-point) triangles))))
  (iter (cadr polygon) (caddr polygon) (cddr polygon) '()))
  