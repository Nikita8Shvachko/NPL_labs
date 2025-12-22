(defpackage :maze-solver
  (:use :cl))
;; Создаем отдельное пространство имен для нашего решателя

(in-package :maze-solver)
;; Начинаем работать внутри этого пространства

;;; --- Обозначения на карте ---
(defconstant +wall+ #\#)       ;; Стена
(defconstant +path+ #\*)       ;; Дорога к выходу (здесь мы прошли)
(defconstant +start+ #\S)      ;; Отсюда начинаем
(defconstant +end+ #\E)        ;; Сюда хотим попасть
(defconstant +empty+ #\Space)  ;; Просто пустая клетка
(defconstant +visited+ #\.)    ;; Тупик (были здесь, но выхода не нашли)

;;; --- Глобальные переменные ---
;; Используем их, чтобы везде видеть нашу карту
(defvar *maze* nil)  ;; Сама карта лабиринта
(defvar *rows* 0)    ;; Сколько строк
(defvar *cols* 0)    ;; Сколько столбцов

(defun read-maze (filename)
  "Читаем лабиринт из файла и превращаем в карту (массив)."
  ;; with-open-file удобно откроет файл и сам закроет, когда закончим
  (let ((lines (with-open-file (stream filename)
                 ;; Бежим по строкам файла, пока они не кончатся
                 (loop for line = (read-line stream nil)
                       while line
                       unless (zerop (length line)) ;; Пустые строки пропускаем
                       collect line))))
    (when lines
      ;; Запоминаем размеры карты
      (setf *rows* (length lines))
      (setf *cols* (length (first lines)))
      ;; Создаем пустую карту нужного размера
      (setf *maze* (make-array (list *rows* *cols*) ;; Создаем массив нужного размера
                               :element-type 'character ;; Тип элементов массива
                               :initial-element +empty+)) ;; Инициализируем массив пустыми клетками
      ;; Переносим символы из файла на нашу карту
      (loop for r from 0 below *rows*
            for line in lines
            do (loop for c from 0 below (min *cols* (length line)) ;; Бежим по столбцам
                     do (setf (aref *maze* r c) (char line c)))) ;; Переносим символы из файла на нашу карту
      *maze*))) ;; Возвращаем массив

(defun find-start ()
  "Ищем, где находится буква S (старт) - точка старта."
  (loop for r from 0 below *rows* do ;; Бежим по строкам
    (loop for c from 0 below *cols* do ;; Бежим по столбцам
      (when (char= (aref *maze* r c) +start+)
        (return-from find-start (list r c)))))) ;; Если нашли старт, возвращаем координаты

(defun solve (r c)
  "Главная функция поиска пути (DFS) - поиск в глубину."
  
  ;; 1. Мы вообще внутри лабиринта? (проверяем границы)
  (unless (array-in-bounds-p *maze* r c)
    (return-from solve nil)) ;; Если не внутри, возвращаем nil

  (let ((cell (aref *maze* r c)))
    ;; 2. Ура, мы нашли выход!
    (when (char= cell +end+)
      (return-from solve t)) ;; Если нашли выход, возвращаем true
    
    ;; 3. Сюда нельзя: или стена, или мы тут уже были (путь или тупик)
    (when (or (char= cell +wall+) 
              (char= cell +path+) 
              (char= cell +visited+))
      (return-from solve nil)) ;; Если стена, путь или тупик, возвращаем nil

    ;; 4. Отмечаем эту клетку как часть пути (*), чтобы не ходить кругами
    ;; (Стартовую позицию S не затираем)
    (unless (char= cell +start+)
      (setf (aref *maze* r c) +path+)) ;; Если не старт, отмечаем клетку как часть пути

    ;; 5. Пробуем пойти во все 4 стороны: Вверх, Вправо, Вниз, Влево
    (let ((directions '((-1 0) (0 1) (1 0) (0 -1)))) ;; Все 4 стороны
      (dolist (dir directions) ;; Бежим по всем сторонам
        (let ((nr (+ r (first dir)))   ;; Новая строка
              (nc (+ c (second dir)))) ;; Новый столбец
          ;; Если из соседней клетки нашли выход -> мы тоже ведем к выходу!
          (when (solve nr nc)
            (return-from solve t))))) ;; Если из соседней клетки нашли выход, возвращаем true

    ;; 6. Если мы здесь, значит все пути отсюда ведут в тупик.
    ;; Ставим метку "тупик" (.), чтобы больше сюда не соваться.
    (unless (char= cell +start+)
      (setf (aref *maze* r c) +visited+)) ;; Если не старт, отмечаем клетку как тупик
    
    ;; Возвращаем "нет", пути отсюда нет
    nil)) ;; Возвращаем nil, пути отсюда нет

(defun clean-maze ()
  "Убираем точки (тупики), оставляем только - * - путь."
  (loop for r from 0 below *rows* do ;; Бежим по строкам
    (loop for c from 0 below *cols* do ;; Бежим по столбцам
      (when (char= (aref *maze* r c) +visited+) ;; Если клетка тупик
        (setf (aref *maze* r c) +empty+))))) ;; Если клетка тупик, заменяем на пустую клетку

(defun print-maze (stream)
  "Печатаем карту (в файл или на экран)."
  (loop for r from 0 below *rows* do ;; Бежим по строкам
    (loop for c from 0 below *cols* do ;; Бежим по столбцам
      (format stream "~c" (aref *maze* r c))) ;; Печатаем символ
    (format stream "~%"))) ;; Печатаем новую строку

(defun main ()
  "Запуск всего процесса."
  (format t "Чтение лабиринта из input.txt...~%")
  
  ;; Проверяем, есть ли файл
  (if (probe-file "input.txt") 
      (progn
        (read-maze "input.txt") ;; Читаем лабиринт из файла
        (let ((start-pos (find-start)))
          (if start-pos ;; Если старт найден
              (progn
                (format t "Старт найден в ~a. Ищем выход...~%" start-pos) ;; Выводим координаты старт
                ;; Запускаем поиск пути! (рекурсивно)
                (if (solve (first start-pos) (second start-pos))
                    (progn
                      (format t "Путь найден!~%") ;; Выводим, что путь найден
                      (clean-maze) ;; Стираем следы тупиков
                      
                      ;; Сохраняем в файл
                      (with-open-file (stream "output.txt" 
                                              :direction :output 
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                        (print-maze stream)) ;; Печатаем карту в файл
                      
                      ;; Показываем на экране
                      (print-maze t) ;; Печатаем карту на экран
                      (format t "Результат записан в output.txt~%")) ;; Выводим, что результат записан в файл
                    ;; Если solve вернул nil
                    (format t "Выхода нет (лабиринт непроходим).~%"))) ;; Выводим, что выхода нет
              ;; Если find-start вернул nil
              (format t "Ошибка: Не могу найти 'S' на карте.~%")))) ;; Выводим, что старт не найден
      ;; Если файла нет
      (format t "Ошибка: файл input.txt не найден.~%"))) ;; Выводим, что файл не найден

;; Поехали!
(main)
