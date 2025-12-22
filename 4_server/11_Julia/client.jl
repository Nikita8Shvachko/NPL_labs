using Sockets

# Конфигурация порта
const PORT = 8082
const HOST = "127.0.0.1"
const INPUT_FILE = "input.txt"
const OUTPUT_FILE = "output.txt"

function run_client()
    try
        # Подключаемся к серверу
        println("Подключение к $HOST:$PORT...")
        sock = connect(HOST, PORT)
        println("Успешно подключено!")

        # Открываем файлы
        input = open(INPUT_FILE, "r") # открываем файл для чтения
        output = open(OUTPUT_FILE, "w")

        for line in eachline(input) # читаем файл построчно
            if isempty(strip(line)) # если строка пустая, то пропускаем
                continue
            end

            println("Отправка: $line") # выводим сообщение о том, что отправляем данные
            
            # Отправляем на сервер (readline на сервере ждет \n)
            println(sock, line)
            
            # Получаем ответ
            response = readline(sock)
            println("Ответ: $response")
            
            # Записываем результат
            println(output, "$line = $response")
        end

        close(input) # закрываем файлы
        close(output)
        close(sock)
        println("Готово! Результат в $OUTPUT_FILE")

    catch e
        println("Ошибка клиента: $e") # выводим сообщение о ошибке
    end
end

run_client()

