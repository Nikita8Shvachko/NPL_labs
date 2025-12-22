# Задача 7: Конвертер XML в CSV на Groovy

Программа для конвертации списка сотрудников из формата XML в плоский формат CSV.

## Описание

Скрипт читает файл `input.xml`, парсит структуру сотрудников (атрибуты и вложенные теги) и сохраняет результат в `output.csv`. Используется стандартный парсер `XmlSlurper`, встроенный в Groovy.

## Требования

- Groovy (версия 2.x или 3.x)
- Java Runtime Environment (JRE)

## Установка и запуск

1.  Убедитесь, что Groovy установлен:
    ```bash
    groovy -version
    ```
2.  Перейдите в директорию проекта:
    ```bash
    cd 3_convertation/Groovy
    ```
3.  Запустите скрипт:
    ```bash
    groovy converter.groovy
    ```

## Формат данных

**Входной (XML):**
```xml
<employees>
  <employee id="1">
    <name>Name</name>
    <role>Role</role>
    <salary>Amount</salary>
  </employee>
</employees>
```

**Выходной (CSV):**
```csv
id,name,role,salary
1,Name,Role,Amount
```

