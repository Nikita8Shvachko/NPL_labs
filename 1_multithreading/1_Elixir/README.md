# Задача 1: Обедающие философы (Elixir) многопоточность на Elixir

Этот проект реализует классическую задачу об обедающих философах, используя модель акторов Elixir (процессы и GenServer).

## Структура
- `philosophers.exs`: Основной скрипт симуляции.
- `input.txt`: Файл конфигурации параметров симуляции.
- `output.txt`: Файл с результатами статистики.

## Конфигурация (input.txt)
Файл `input.txt` должен содержать параметры в формате `ключ=значение`:
```properties
number_of_philosophers=5
simulation_duration_ms=10000
min_think_time_ms=500
max_think_time_ms=1000
min_eat_time_ms=500
max_eat_time_ms=1000
```

## Статистика (output.txt)
После завершения симуляции в файл `output.txt` записывается статистика по каждому философу:
- Сколько раз он поел.
- Среднее время ожидания (от момента возникновения голода до начала еды).

## Запуск

нужен Elixir!

```bash
cd 1_multithreading/Elixir
elixir philosophers.exs
```
