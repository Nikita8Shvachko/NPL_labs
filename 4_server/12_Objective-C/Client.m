#import <Foundation/Foundation.h>
#import <netinet/in.h>
#import <sys/socket.h>
#import <arpa/inet.h>

// Конфигурация порта
#define PORT 8081
#define HOST "127.0.0.1"
#define BUFFER_SIZE 4096 // размер буфера

int main(int argc, const char * argv[]) {
    @autoreleasepool { // инициализируем автореleasepool для управления памятью
        // пути к файлам
        NSString *inputPath = @"input.txt"; // путь к входному файлу
        NSString *outputPath = @"output.txt"; // путь к выходному файлу
        
        // создаем сокет
        int sock = socket(AF_INET, SOCK_STREAM, 0); // создаем сокет
        if (sock < 0) { // если сокет не создан, то выводим сообщение о ошибке и выходим из программы
            perror("Socket creation failed");
            return 1;
        }
        
        struct sockaddr_in serv_addr; // структура для хранения адреса сервера
        serv_addr.sin_family = AF_INET; // устанавливаем семейство адресов
        serv_addr.sin_port = htons(PORT); // устанавливаем порт
        
        if (inet_pton(AF_INET, HOST, &serv_addr.sin_addr) <= 0) { // если адрес некорректный, то выводим сообщение о ошибке и выходим из программы
            perror("Invalid address"); // выводим сообщение о ошибке
            return 1;
        }
        
        // 2. Подключение
        printf("Подключение к %s:%d...\n", HOST, PORT); // выводим сообщение о том, что подключаемся к серверу
        if (connect(sock, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) { // если не удалось подключиться, то выводим сообщение о ошибке и выходим из программы
            perror("Connection Failed");
            return 1; // выходим из программы
        }
        printf("Подключено!\n"); // выводим сообщение о том, что подключено к серверу
        
        // 3. Чтение файла
        NSError *error = nil; // инициализируем ошибку
        NSString *fileContent = [NSString stringWithContentsOfFile:inputPath encoding:NSUTF8StringEncoding error:&error];
        
        if (error) {
            printf("Ошибка чтения файла: %s\n", [[error localizedDescription] UTF8String]); // выводим сообщение о ошибке
            return 1;
        }
        
        NSArray *lines = [fileContent componentsSeparatedByString:@"\n"]; // разделяем файл на строки
        NSMutableString *outputContent = [NSMutableString string]; // инициализируем строку для вывода результата
        char buffer[BUFFER_SIZE];
        
        for (NSString *line in lines) {
            NSString *trimmedLine = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]]; // убираем пробелы и переносы строк с краев
            if ([trimmedLine length] == 0) continue;
            
            printf("Отправка: %s\n", [trimmedLine UTF8String]); // выводим сообщение о том, что отправляем строку   
            
            // отправляем строку на сервер
            const char *msg = [trimmedLine UTF8String];
            send(sock, msg, strlen(msg), 0);
            
            // Получение ответа
            ssize_t valread = read(sock, buffer, BUFFER_SIZE - 1); // читаем ответ от сервера
            if (valread > 0) { // если ответ получен, то выводим его
                buffer[valread] = '\0'; // завершаем строку
                NSString *response = [NSString stringWithUTF8String:buffer]; // преобразуем ответ в строку
                printf("Ответ: %s\n", [response UTF8String]); // выводим ответ
                
                [outputContent appendFormat:@"%@ = %@\n", trimmedLine, response]; // добавляем ответ в строку для вывода результата
            }
        }
        
        // 4. Запись результата
        [outputContent writeToFile:outputPath atomically:YES encoding:NSUTF8StringEncoding error:&error]; // записываем результат в файл
        if (error) {
            printf("Ошибка записи файла\n"); // выводим сообщение о ошибке
        } else {
            printf("Готово! Результат в %s\n", [outputPath UTF8String]); // выводим сообщение о том, что результат записан в файл
        }
        
        close(sock);
    } // закрываем соединение с сервером
    return 0;
}

