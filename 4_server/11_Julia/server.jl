using Sockets

# Конфигурация порта
const PORT = 8082

# функция шифрования Цезаря
function caesar_cipher(text::AbstractString, shift::Int) # текст и сдвиг
    result = IOBuffer() # буфер для хранения результата
    
    for char in text
        if isletter(char) # если символ является буквой
            # определяем базу (а или A)
            base = islowercase(char) ? 'a' : 'A'
            # (char code - base code + shift) % 26 + base code
            # Julia использует 1-based indexing, но для модуля удобнее 0-based
            offset = Int(char) - Int(base) # смещение
            new_offset = mod(offset + shift, 26) # новый смещение
            new_char = Char(Int(base) + new_offset) # новый символ
            print(result, new_char) # записываем символ в буфер
        else
            # остальные символы оставляем без изменений
            print(result, char) # записываем символ в буфер
        end
    end
    
    return String(take!(result)) # преобразуем буфер в строку и возвращаем результат
end

function start_server()
    server = listen(PORT) # создаем сервер на порту
    println("Сервер запущен на порту $PORT...")

    try
        while true
            sock = accept(server) # принимаем входящее подключение
            println("Клиент подключен")
            
            # обработка клиента в асинхронной задаче (корутине)
            @async begin
                try
                    while isopen(sock)
                        # читаем строку
                        line = readline(sock)
                        if isempty(line) # если строка пустая, то пропускаем
                            break
                        end
                        
                        println("Получено: $line") # выводим сообщение о том, что получено сообщение
                        
                        # парсим запрос: "SHIFT|TEXT"
                        parts = split(line, "|", limit=2)
                        
                        response = "" # инициализируем ответ
                        if length(parts) == 2
                            try
                                shift = parse(Int, strip(parts[1])) # преобразуем сдвиг в число
                                text = strip(parts[2]) # очищаем текст от \r и пробелов
                                response = caesar_cipher(text, shift) # шифруем текст
                            catch e
                                response = "ERROR: Invalid shift number" # выводим сообщение о ошибке
                            end
                        else
                            response = "ERROR: Invalid format. Use SHIFT|TEXT" # выводим сообщение о ошибке
                        end
                        
                        println("Отправляю: $response") # выводим сообщение о том, что отправляем ответ
                        println(sock, response) # отправляем ответ клиенту
                    end
                catch e
                    println("Ошибка обработки клиента: $e") # выводим сообщение о ошибке    
                finally
                    close(sock) # закрываем соединение с клиентом
                    println("Клиент отключен") # выводим сообщение о том, что клиент отключен
                end
            end
        end
    catch e
        if e isa InterruptException # если прерывание, то выводим сообщение о том, что сервер остановлен
            println("\nСервер остановлен") # выводим сообщение о том, что сервер остановлен
        else
            rethrow(e) # выводим сообщение о ошибке и выходим из цикла
        end
    finally
        close(server) # закрываем сервер
    end
end

# Запуск сервера
start_server()

