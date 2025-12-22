module Main where

import Data.List (intercalate)

-- | Проверяет, безопасна ли позиция для новой королевы.
-- Параметры:
--   queens: список столбцов уже установленных королев (хранится в обратном порядке).
--   row:    номер текущей строки, куда хотим поставить новую королеву.
--   col:    номер столбца, который проверяем.
--
-- Логика:
--   Мы проходим по всем уже установленным королевам (zip [0..] ...) сопоставляет им их строки.
--   Для каждой предыдущей королевы проверяем два условия:
--     1. Вертикаль: c /= col (не в том же столбце).
--     2. Диагональ: abs (row - r) /= abs (col - c) (разница координат по X не равна разнице по Y).
isSafe :: [Int] -> Int -> Int -> Bool
isSafe queens row col = all safe (zip [0..] (reverse queens))
  where
    -- r - строка уже установленной королевы, c - её столбец
    safe (r, c) = c /= col && abs (row - r) /= abs (col - c)

-- | Основная функция для поиска всех решений задачи N ферзей.
-- Возвращает список решений, где каждое решение — это список координат столбцов.
solveNQueens :: Int -> [[Int]]
solveNQueens n = placeQueens n 0 []
  where
    -- Вспомогательная рекурсивная функция:
    --   size:          общий размер доски (N)
    --   row:           номер строки, которую сейчас заполняем
    --   currentQueens: список позиций уже поставленных ферзей (аккумулятор)
    placeQueens size row currentQueens
      -- Базовый случай: если номер строки равен размеру доски, значит мы успешно расставили всех ферзей.
      -- Возвращаем текущую конфигурацию (переворачиваем список, так как собирали его с конца).
      | row == size = [reverse currentQueens]
      
      -- Рекурсивный шаг:
      -- 1. Генерируем возможные столбцы: [0 .. size - 1].
      -- 2. Для каждого столбца проверяем isSafe.
      -- 3. Если безопасно -> запускаем рекурсию для следующей строки (row + 1).
      -- 4. concatMap собирает все найденные решения из разных веток в один общий список.
      | otherwise = concatMap (\col ->
          if isSafe currentQueens row col
          then placeQueens size (row + 1) (col : currentQueens)
          else [] -- Если небезопасно, возвращаем пустой список (тупиковая ветвь)
        ) [0 .. size - 1]

-- | Форматирует одно решение в красивый строковый вид (сетка с Q и точками).
formatSolution :: Int -> [Int] -> String
formatSolution n solution = unlines [rowStr c | c <- solution]
  where
    -- Генерирует строку: 'Q ' если ферзь, '. ' если пусто
    rowStr col = unwords [if i == col then "Q" else "." | i <- [0 .. n - 1]]

-- | Точка входа в программу.
-- Читает N из файла, находит решения и записывает их в output.txt.
main :: IO ()
main = do
    -- 1. Чтение входных данных
    inputContent <- readFile "input.txt"
    -- Берем первую строку и превращаем её в число (Int)
    let n = read (head (lines inputContent)) :: Int
    
    -- 2. Запуск алгоритма
    let solutions = solveNQueens n
    let numSolutions = length solutions -- Считаем количество найденных решений
    
    -- 3. Формирование текста для записи
    let header = "Найдено решений для N=" ++ show n ++ ": " ++ show numSolutions ++ "\n"
    
    -- zipWith нумерует решения (1, 2, 3...) и применяет к каждому formatSolution
    let solutionsStr = zipWith (\i sol -> "Решение " ++ show (i + 1) ++ ":\n" ++ formatSolution n sol) [0..] solutions
    
    -- Соединяем все решения через пустую строку
    let outputContent = header ++ "\n" ++ intercalate "\n" solutionsStr
    
    -- 4. Запись результата в файл
    writeFile "output.txt" outputContent
    putStrLn $ "Готово! Найдено решений: " ++ show numSolutions ++ ". Результат в файле output.txt"
