#import <Foundation/Foundation.h>
#import <netinet/in.h>
#import <sys/socket.h>
#import <arpa/inet.h>

#define PORT 8081
#define BUFFER_SIZE 4096

// Функция обработки одного клиента
void handle_client(int client_socket) {
    char buffer[BUFFER_SIZE]; // буфер для приема данных
    
    while (1) {
        // Читаем данные
        ssize_t bytes_read = recv(client_socket, buffer, BUFFER_SIZE - 1, 0);
        
        if (bytes_read <= 0) {
            printf("Клиент отключился.\n"); // выводим сообщение о том, что клиент отключился
            break;
        }
        
        buffer[bytes_read] = '\0'; // завершаем строку
        
        // Преобразуем в NSString 
        NSString *request = [NSString stringWithUTF8String:buffer]; // преобразуем строку в NSString
        // Убираем пробелы и переносы строк с краев
        request = [request stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]]; // убираем пробелы и переносы строк с краев
        
        printf("Получено: %s\n", [request UTF8String]); // выводим полученную строку
        
        if ([request length] == 0) continue; // если строка пустая, то пропускаем

        // --- ЛОГИКА СОРТИРОВКИ ---
        
        // 1. Разбиваем на слова
        NSArray *words = [request componentsSeparatedByString:@" "]; // разбиваем строку на слова
        
        // 2. Сортируем слова (case-insensitive)
        NSArray *sortedWords = [words sortedArrayUsingSelector:@selector(caseInsensitiveCompare:)]; // сортируем слова
        
        // 3. Обрабатываем каждое слово (сортируем буквы внутри)
        NSMutableArray *processedWords = [NSMutableArray array]; // инициализируем массив для обработки слов
        
        for (NSString *word in sortedWords) {
            if ([word length] == 0) continue; // если слово пустое, то пропускаем
            
            // Превращаем слово в массив символов для сортировки
            NSMutableArray *characters = [NSMutableArray array]; // инициализируем массив для символов
            for (int i = 0; i < [word length]; i++) { // проходим по каждому символу в слове
                NSString *charStr = [word substringWithRange:NSMakeRange(i, 1)]; // получаем символ из слова
                [characters addObject:charStr]; // добавляем символ в массив
            }
            
            // Сортируем буквы
            [characters sortUsingSelector:@selector(caseInsensitiveCompare:)]; // сортируем символы
            
            // Собираем обратно в слово
            NSString *sortedCharsWord = [characters componentsJoinedByString:@""]; // собираем символы обратно в слово
            [processedWords addObject:sortedCharsWord]; // добавляем слово в массив для обработки слов
        }
        
        // 4. Собираем предложение обратно
        NSString *response = [processedWords componentsJoinedByString:@" "]; // собираем слова обратно в предложение
        printf("Отправляю: %s\n", [response UTF8String]); // выводим отправленное предложение
        
        // Отправляем ответ
        const char *responseBytes = [response UTF8String]; // преобразуем предложение в строку
        send(client_socket, responseBytes, strlen(responseBytes), 0); // отправляем ответ клиенту
    }
    
    close(client_socket); // закрываем соединение с клиентом
}

int main(int argc, const char * argv[]) {
    @autoreleasepool { // инициализируем автореleasepool для управления памятью
        int server_socket, client_socket; // создаем сокеты
        struct sockaddr_in server_addr, client_addr; // структура для хранения адреса сервера и клиента
        socklen_t client_len = sizeof(client_addr); // размер клиента
        
        // Создаем сокет
        server_socket = socket(AF_INET, SOCK_STREAM, 0); // создаем сокет
        if (server_socket < 0) {
            perror("Ошибка создания сокета"); // выводим сообщение о ошибке
            return 1;
        }
        
        // Опция REUSEADDR
        int opt = 1;
        setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)); // устанавливаем опцию REUSEADDR
        
        // Настройка адреса
        server_addr.sin_family = AF_INET; // устанавливаем семейство адресов
        server_addr.sin_addr.s_addr = INADDR_ANY; // устанавливаем адрес сервера
        server_addr.sin_port = htons(PORT); // устанавливаем порт
        
        // Bind
        if (bind(server_socket, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) { // если не удалось привязать сокет, то выводим сообщение о ошибке и выходим из программы
            perror("Ошибка bind");
            return 1; // выходим из программы
        }
        
        // Listen
        listen(server_socket, 5); // слушаем сокет
        printf("Сервер запущен на порту %d...\n", PORT);
        
        while (1) {
            client_socket = accept(server_socket, (struct sockaddr *)&client_addr, &client_len); // принимаем клиента
            if (client_socket < 0) {
                perror("Ошибка accept"); // выводим сообщение о ошибке и продолжаем слушать сокет
                continue;
            }
            
            printf("Новое подключение\n"); // выводим сообщение о том, что новое подключение
            handle_client(client_socket); // обрабатываем клиента
        }
        
        close(server_socket); // закрываем соединение с сервером
    }
    return 0;
}

