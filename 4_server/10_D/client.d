module client;

import std.socket;
import std.stdio;
import std.string;
import std.file; // Для чтения/записи файлов
import core.thread;
import core.time;

// Конфигурация порта
const ushort PORT = 8080;
const string HOST = "127.0.0.1";
const string INPUT_FILE = "input.txt";
const string OUTPUT_FILE = "output.txt";

void main() {
    try {
        // 1. Читаем входной файл построчно
        if (!exists(INPUT_FILE)) { // если файл не найден, то выводим сообщение о ошибке
            writeln("Ошибка: Файл ", INPUT_FILE, " не найден.");
            return;
        }
        
        auto inputFile = File(INPUT_FILE, "r"); // открываем файл для чтения
        auto outputFile = File(OUTPUT_FILE, "w"); // открываем файл для записи
        
        // 2. Подключаемся к серверу
        writeln("Подключение к ", HOST, ":", PORT, "...");
        auto socket = new TcpSocket(new InternetAddress(HOST, PORT)); // создаем сокет и подключаемся к серверу
        writeln("Успешно подключено!");

        // 3. Обработка каждой строки
        foreach (line; inputFile.byLine()) { // читаем файл построчно
            string text = line.strip().idup; // idup делает копию (immutable dup)
            if (text.length == 0) continue; // если строка пустая, то пропускаем

            writeln("Отправка: ", text);
            
            // Отправляем данные
            socket.send(text); // отправляем данные на сервер

            // Получаем ответ
            char[1024] buffer;
            auto received = socket.receive(buffer); // получаем ответ от сервера
            
            if (received > 0) {
                string response = buffer[0 .. received].idup; // преобразуем данные в строку
                writeln("Ответ: ", response);
                
                // Записываем в файл: "исходное = ответ"
                outputFile.writeln(text, " = ", response); // записываем данные в файл
            }

            // Задержка перед следующим запросом
            Thread.sleep(dur!("seconds")(1));
        }
        
        // Закрываем ресурсы
        socket.close(); // закрываем сокет
        outputFile.close(); // закрываем файл
        writeln("Готово! Результаты в ", OUTPUT_FILE);

    } catch (Exception e) {
        writeln("Ошибка клиента: ", e.msg); // выводим сообщение о ошибке
    }
}

