-module(matrix_mult).
-export([main/0]).

%% Точка входа в программу
main() ->
    try
        %% Чтение матриц из файла input.txt
        {MatrixA, MatrixB} = read_input("input.txt"),
        
        %% Выводим размер считанной матрицы A
        io:format("Считана матрица A: ~p~n", [mat_size(MatrixA)]),
        %% Если матрица небольшая, выводим её содержимое
        maybe_print_matrix("Матрица A:", MatrixA),

        %% Выводим размер считанной матрицы B
        io:format("Считана матрица B: ~p~n", [mat_size(MatrixB)]),
        %% Если матрица небольшая, выводим её содержимое
        maybe_print_matrix("Матрица B:", MatrixB),
        
        %% Выполнение умножения матриц
        Result = multiply(MatrixA, MatrixB),
        
        %% Запись результата в файл output.txt
        write_output("output.txt", Result),
        %% Сообщаем об успехе
        io:format("Готово. Результат записан в output.txt~n"),
        
        %% Если результат небольшой, показываем его
        maybe_print_matrix("Результат:", Result)
    catch
        %% Обработка ошибок (например, несовпадение размерностей)
        error:Reason ->
            io:format("Ошибка: ~p~n", [Reason]),
            init:stop(1);
        %% Обработка выброшенных исключений
        throw:Reason ->
            io:format("Ошибка: ~p~n", [Reason]),
            init:stop(1)
    end,
    %% Завершаем работу программы
    init:stop().

%% Вспомогательная функция для получения размеров матрицы (строки, столбцы)
mat_size(M) ->
    %% length(M) - количество строк
    %% length(hd(M)) - длина первой строки (количество столбцов)
    {length(M), length(hd(M))}.

%% Вывод матрицы в консоль, если она небольшая (до 10x10)
maybe_print_matrix(Label, Matrix) ->
    %% Получаем размеры матрицы
    {Rows, Cols} = mat_size(Matrix),
    if 
        %% Если строк и столбцов <= 10
        Rows =< 10, Cols =< 10 ->
            io:format("~ts~n", [Label]),
            print_matrix(Matrix);
        %% Иначе ничего не делаем
        true -> 
            ok
    end.

%% Функция печати матрицы
print_matrix(Matrix) ->
    %% Проходим по каждой строке матрицы
    lists:foreach(fun(Row) ->
        %% Преобразуем числа в строки и соединяем их табуляцией
        Line = string:join([integer_to_list(X) || X <- Row], "\t"),
        %% Печатаем строку
        io:format("~s~n", [Line])
    end, Matrix).

%% Чтение входного файла и парсинг данных
read_input(Filename) ->
    %% Читаем весь файл в бинарном формате
    {ok, Binary} = file:read_file(Filename),
    %% Преобразуем бинарные данные в строку
    String = binary_to_list(Binary),
    %% Разбиваем строку на числа, используя разделители
    Tokens = string:tokens(String, "\r\n\t "),
    %% Преобразуем список чисел в список целых чисел
    Ints = [list_to_integer(T) || T <- Tokens],
    %% Разбираем список чисел на две матрицы
    parse_data(Ints).

%% Разбираем список чисел на две матрицы
parse_data([R1, C1 | Rest]) ->
    %% Извлекаем первую матрицу размером R1 x C1
    {MatA, Rest2} = take_matrix(R1, C1, Rest),
    %% Извлекаем размеры второй матрицы
    [R2, C2 | Rest3] = Rest2,
    
    %% Проверка совместимости размеров матриц для умножения (столбцы A == строки B)
    if C1 =/= R2 ->
        throw({dimension_mismatch, C1, R2});
    true -> ok %% Если размеры совпадают, продолжаем
    end,
    
    %% Извлекаем вторую матрицу
    {MatB, []} = take_matrix(R2, C2, Rest3),
    %% Возвращаем кортеж из двух матриц
    {MatA, MatB}.

%% Извлекаем матрицу заданного размера из списка данных
take_matrix(Rows, Cols, Data) ->
    %% Общее количество элементов в матрице
    Count = Rows * Cols,
    %% Отделяем нужное количество элементов от остального списка
    {Flat, Rest} = lists:split(Count, Data),
    %% Разбиваем плоский список на строки по Cols элементов
    Matrix = partition(Cols, Flat),
    %% Возвращаем готовую матрицу и остаток данных
    {Matrix, Rest}.

%% Рекурсивно разбиваем плоский список на строки заданной длины
partition(_, []) -> [];
partition(N, List) ->
    %% Отделяем первые N элементов (одну строку)
    {Head, Tail} = lists:split(N, List),
    %% Добавляем эту строку к результату
    [Head | partition(N, Tail)].

%% Основная функция параллельного умножения матриц
multiply(A, B) ->
    %% Транспонируем матрицу B, чтобы столбцы стали строками (удобнее для чтения)
    BT = transpose(B),
    %% Запоминаем PID текущего процесса
    Parent = self(),
    
    %% Запускаем процессы вычислений параллельно
    %% lists:zip нумерует строки матрицы A (добавляет индексы)
    Pids = [spawn(fun() -> 
                %% Вычисляем строку результата:
                %% Берем текущую строку RowA и умножаем её скалярно на каждый столбец из BT
                RowRes = [dot_product(RowA, ColB) || ColB <- BT],
                
                %% Отправляем сообщение текущему процессу
                %% {PID процесса, Индекс строки, Результат строки}
                Parent ! {self(), Index, RowRes}
            end) || {Index, RowA} <- lists:zip(lists:seq(1, length(A)), A)],
    
    %% Сбор результатов от всех запущенных процессов
    Results = [receive {Pid, I, Res} -> {I, Res} end || Pid <- Pids],
    
    %% Сортировка результатов по индексу (I), так как процессы могли ответить в разном порядке
    Sorted = lists:sort(Results),
    
    %% Извлекаем только сами строки (отбрасываем индексы)
    [R || {_, R} <- Sorted].

%% Транспонирование матрицы (меняем строки и столбцы местами) 
transpose([[]|_]) -> [];
transpose(M) ->
    %% Берем первые элементы всех списков (первый столбец) -> становится первой строкой
    %% Рекурсивно обрабатываем хвосты списков
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

%% Вычисление скалярного произведения двух векторов (строки и столбца)
dot_product(Row, Col) ->
    %% Попарно умножаем элементы и суммируем результаты
    lists:sum([X*Y || {X,Y} <- lists:zip(Row, Col)]).

%% Запись матрицы в выходной файл
write_output(Filename, Matrix) ->
    %% Открываем файл для записи
    {ok, File} = file:open(Filename, [write]),
    %% Проходим по всем строкам матрицы
    lists:foreach(fun(Row) ->
        %% Преобразуем строку чисел в строку текста с пробелами
        Line = string:join([integer_to_list(X) || X <- Row], " "),
        %% Записываем строку в файл с переводом каретки
        io:format(File, "~s~n", [Line])
    end, Matrix),
    %% Закрываем файл
    file:close(File).
