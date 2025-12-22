# Задача 12: TCP Сервер на Objective-C

Клиент-серверное приложение на Objective-C. Сервер принимает предложение, сортирует слова по алфавиту, а также сортирует буквы внутри каждого слова.

## Требования
- Компилятор `clang` (или `gcc` с поддержкой Objective-C)
- Фреймворк `Foundation` (стандартный для macOS)

## Компиляция

Используйте clang с флагами для линковки Foundation:

```bash
clang -fobjc-arc -framework Foundation Server.m -o server
clang -fobjc-arc -framework Foundation Client.m -o client
```

*Флаг `-fobjc-arc` включает автоматический подсчет ссылок (ARC), чтобы не управлять памятью вручную (`retain`/`release`).*

## Запуск

1. Запустите сервер:
   ```bash
   ./server
   ```
2. В другом окне запустите клиент:
   ```bash
   ./client
   ```
3. Проверьте `output.txt`.

