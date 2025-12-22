module server;

import std.socket;
import std.stdio;
import std.string;
import std.algorithm;
import std.array;
import std.conv;
import std.range;
import std.uni : toLower;
import core.thread;
import core.time;

// Конфигурация порта
const ushort PORT = 8080;

void main() {
    try {
        // создаем TCP сокет (IPv4)
        auto listener = new TcpSocket();
        // устанавливаем опцию переиспользования адреса (чтобы быстро перезапускать сервер)
        listener.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, true);
        
        listener.bind(new InternetAddress(PORT)); // привязываем сокет к адресу и порту
        listener.listen(10); // очередь на 10 подключений

        writeln("Сервер запущен на порту ", PORT);
        writeln("Ожидание подключений...");

        while (true) {
            Socket clientSocket = listener.accept(); // принимаем входящее подключение
            writeln("Клиент подключен: ", clientSocket.remoteAddress());

            handleClient(clientSocket); // обрабатываем клиента в том же потоке 
        }

    } catch (Exception e) {
        writeln("Ошибка сервера: ", e.msg);
    }
}

void handleClient(Socket client) {
    // буфер для приема данных
    char[1024] buffer;
    
    while (true) {
        auto received = client.receive(buffer); // получаем данные от клиента
        
        // если 0 - клиент отключился, Socket.ERROR - ошибка
        if (received == 0 || received == Socket.ERROR) { 
            writeln("Клиент отключился"); // выводим сообщение о том, что клиент отключился и выходим из цикла
            break;
        }

        // Преобразуем байты в строку и убираем лишние пробелы/переводы строк
        string request = buffer[0 .. received].idup.strip(); // преобразуем данные в строку и убираем лишние пробелы/переводы строк
        writeln("Получено: ", request);

        // --- Логика обработки ---
        
        string reversed = request.retro.text; // разворачиваем строку
        

        string cleanRequest = request.toLower(); // преобразуем строку в нижний регистр
        string cleanReversed = cleanRequest.retro.text; // разворачиваем строку
        
        if (cleanRequest == cleanReversed && request.length > 0) {
            // если палиндром - добавляем сюрприз
            reversed ~= " - palindrome surprise!";
        }

        writeln("Отправляю: ", reversed); // выводим сообщение о том, что отправляем ответ
        
        // Задержка перед отправкой ответа
        Thread.sleep(dur!("seconds")(1));

        client.send(reversed); // отправляем ответ клиенту
    }
    
    client.close(); // закрываем соединение с клиентом
}

