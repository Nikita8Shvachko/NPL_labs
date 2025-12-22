# Модуль Вилки (Fork)
defmodule Fork do
  # Используем GenServer для создания процесса-сервера
  use GenServer

  # Genserver - это процесс, который может обрабатывать запросы и сообщения.
  # Запускаем процесс вилки
  def start_link(id) do
    # Регистрируем вилку с глобальным именем
    GenServer.start_link(__MODULE__, id, name: via_tuple(id))
  end

  # Попытка взять вилку (синхронно)
  def grab(fork_id) do
    # Отправляем запрос :grab и ждем ответа
    GenServer.call(via_tuple(fork_id), :grab)
  end

  # Освободить вилку (асинхронно)
  def release(fork_id) do
    # Отправляем команду :release, ответ не нужен
    GenServer.cast(via_tuple(fork_id), :release)
  end

  # Генерируем уникальное имя для процесса вилки
  defp via_tuple(id), do: {:global, {:fork, id}}

  @impl true
  # Инициализация процесса вилки
  def init(id) do
    # Начальное состояние: вилка свободна
    {:ok, %{id: id, status: :free}}
  end

  @impl true
  # Обработка запроса на взятие вилки
  def handle_call(:grab, _from, state) do
    # Проверяем статус вилки
    case state.status do
      :free ->
        # Вилка свободна -> отдаем её и помечаем как занятую
        {:reply, :ok, %{state | status: :taken}}

      :taken ->
        # Вилка занята -> говорим "пробуй позже", состояние не меняем
        {:reply, :retry, state}
    end
  end

  # Аннотация, указывающая, что это реализация колбэка GenServer
  @impl true
  # Обработка команды на освобождение вилки
  def handle_cast(:release, state) do
    # Помечаем вилку как свободную
    {:noreply, %{state | status: :free}}
  end
end

# Модуль Философа (Philosopher)
defmodule Philosopher do
  use GenServer

  # Запускаем философа
  def start_link(config) do
    # Регистрируем процесс философа
    GenServer.start_link(__MODULE__, config, name: via_tuple(config.id))
  end

  # Получить статистику от философа
  def get_stats(id) do
    # Спрашиваем статистику синхронно
    GenServer.call(via_tuple(id), :get_stats)
  end

  def stop(id) do
    # Останавливаем процесс
    GenServer.stop(via_tuple(id))
  end

  # Уникальное имя для философа
  defp via_tuple(id), do: {:global, {:philosopher, id}}

  @impl true
  # Инициализация процесса философа
  def init(config) do
    # Сразу отправляем себе команду "думать"
    send(self(), :think)

    # Объединяем конфигурацию с начальным состоянием
    initial_state =
      Map.merge(config, %{
        # Сколько раз поел
        eat_count: 0,
        # Общее время ожидания еды
        total_wait_time: 0,
        # Когда начал хотеть есть
        wait_start_time: nil
      })

    {:ok, initial_state}
  end

  # Состояние: РАЗМЫШЛЕНИЕ
  @impl true
  # Обработка команды "думать"
  def handle_info(:think, state) do
    # Логируем размышление
    log(state.id, "Размышляет")
    # Имитируем размышление (сон)
    sleep_random(state.min_think, state.max_think)
    # Подумал -> проголодался
    send(self(), :hungry)
    # Возвращаем состояние
    {:noreply, state}
  end

  # Состояние: ГОЛОД
  # Обработка команды "голоден"
  def handle_info(:hungry, state) do
    # Логируем голод
    log(state.id, "Голоден")
    # Засекаем время начала ожидания
    wait_start = System.monotonic_time(:millisecond)
    # Обновляем состояние
    new_state = %{state | wait_start_time: wait_start}
    # Пытаемся поесть
    final_state = try_eat(new_state)
    # Возвращаем обновленное состояние
    {:noreply, final_state}
  end

  # Повторная попытка поесть (если вилки были заняты)
  def handle_info(:retry_eat, state) do
    # Снова пробуем взять вилки
    {:noreply, try_eat(state)}
  end

  # Состояние: ЗАКОНЧИЛ ЕСТЬ
  # Обработка команды "закончил есть"
  def handle_info(:eating_done, state) do
    # Логируем окончание еды
    log(state.id, "Закончил есть, кладет вилки")
    # Кладем правую вилку
    Fork.release(state.right_fork)
    # Кладем левую вилку
    Fork.release(state.left_fork)
    # Снова начинаем думать
    send(self(), :think)
    # Возвращаем состояние
    {:noreply, state}
  end

  # Отдаем статистику по запросу
  @impl true
  # Обработка запроса на получение статистики
  def handle_call(:get_stats, _from, state) do
    # Считаем среднее время ожидания
    avg_wait =
      if state.eat_count > 0 do
        # Считаем среднее
        state.total_wait_time / state.eat_count
      else
        # Если ни разу не ел, возвращаем 0.0
        0.0
      end

    # Возвращаем ответ и состояние
    {:reply, {state.eat_count, avg_wait}, state}
  end

  # Логика взятия вилок (Deadlock Prevention)
  defp try_eat(state) do
    # 1. Пытаемся взять ЛЕВУЮ вилку (синхронный вызов к процессу вилки).
    case Fork.grab(state.left_fork) do
      :ok ->
        # Успех с левой вилкой.
        log(state.id, "Взял левую вилку (#{state.left_fork})")

        # 2. Теперь пытаемся взять ПРАВУЮ вилку.
        case Fork.grab(state.right_fork) do
          :ok ->
            # УСПЕХ: Обе вилки у нас!

            # Считаем, сколько времени прошло с момента :hungry.
            current_time = System.monotonic_time(:millisecond)
            # Сколько ждали?
            wait_time = current_time - state.wait_start_time

            # Голодный философ ест, время ожидания увеличивается.
            log(
              state.id,
              "Взял правую вилку (#{state.right_fork}) -> КУШАЕТ (ждал #{wait_time}мс)"
            ) # Логируем, что философ ест

            time_to_eat = :rand.uniform(state.max_eat - state.min_eat + 1) + state.min_eat - 1
            # Едим (ставим таймер)
            Process.send_after(self(), :eating_done, time_to_eat)

            %{
              state
              | eat_count: state.eat_count + 1,
                total_wait_time: state.total_wait_time + wait_time,
                wait_start_time: nil
            } # Обновляем статистику и состояние

          # Голодный философ закончил есть, время ожидания сбрасывается.
          :retry ->
            # Правую взять не вышло -> кладем левую обратно (иначе будет тупик/deadlock)
            log(state.id, "Не смог взять правую вилку (#{state.right_fork}), кладет левую и ждет")
            Fork.release(state.left_fork)
            # Попробуем позже
            Process.send_after(self(), :retry_eat, random_delay()) # Планируем повторную попытку через случайное время
            state # Возвращаем состояние
        end

      # Голодный философ не смог взять правую вилку, кладет левую обратно и ждет.
      :retry ->
        # Даже левую взять не вышло -> ждем
        Process.send_after(self(), :retry_eat, random_delay())
        state # Возвращаем состояние
    end
  end

  #  философ спит случайное время между min и max миллисекундами.
  defp sleep_random(min, max) do
    # Случайное число в диапазоне
    time = :rand.uniform(max - min + 1) + min - 1
    :timer.sleep(time) # Спим случайное время
  end

  # Случайная задержка
  defp random_delay do
    # Случайная задержка 10-110 мс
    :rand.uniform(100) + 10
  end

  # Логирование с идентификатором философа и сообщением
  defp log(id, message) do
    IO.puts("[Философ #{id}] #{message}")
  end
end

defmodule SimulationConfig do # Модуль для загрузки конфигурации из файла
  # Загрузка конфигурации из файла
  def load(filename) do
    # Читаем файл настроек
    File.read!(filename)
    # Делим на строки
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      # Парсим "ключ=значение"
      [key, value] = String.split(line, "=")
      {String.to_atom(String.trim(key)), String.to_integer(String.trim(value))}
    end)
    # Превращаем в словарь
    |> Map.new()
  end
end

defmodule Simulation do
  # Запуск симуляции
  def run do
    # Включаем поддержку кириллицы в консоли
    :io.setopts(:standard_io, encoding: :unicode)

    # Путь к файлу конфигурации
    input_file = Path.join(__DIR__, "input.txt")
    # Путь к файлу вывода
    output_file = Path.join(__DIR__, "output.txt")

    # Загружаем конфиг
    config = SimulationConfig.load(input_file)
    # Количество философов
    n = config.number_of_philosophers

    IO.puts("Загружена конфигурация из #{input_file}:")
    # Выводим конфигурацию
    IO.inspect(config) # Выводим конфигурацию в консоль

    IO.puts("Запуск симуляции с #{n} философами на #{config.simulation_duration_ms} мс...")

    # Запускаем симуляцию с количеством философов и временем симуляции
    # Создаем вилки (n штук) для философов
    for i <- 1..n do
      Fork.start_link(i)
    end

    # Создаем философов (n штук) с конфигурацией
    for i <- 1..n do
      left_fork = i
      # Последний философ берет 1-ю вилку (замыкает круг)
      right_fork = if i == n, do: 1, else: i + 1

      # Конфигурация для конкретного философа
      philosopher_config = %{
        id: i,
        left_fork: left_fork,
        right_fork: right_fork,
        min_think: config.min_think_time_ms,
        max_think: config.max_think_time_ms,
        min_eat: config.min_eat_time_ms,
        max_eat: config.max_eat_time_ms
      }

      # Запускаем философа с конфигурацией
      Philosopher.start_link(philosopher_config)
    end

    # Ждем завершения времени симуляции
    :timer.sleep(config.simulation_duration_ms) # Спим время симуляции

    IO.puts("\nСимуляция завершена. Сбор статистики...")

    # Собираем статистику
    stats_content =
      for i <- 1..n do
        # Собираем данные
        {count, avg_wait} = Philosopher.get_stats(i) # Получаем статистику у философа

        line =
          "Философ #{i}: Поел #{count} раз(а), Среднее ожидание: #{Float.round(avg_wait, 2)} мс"
        # Выводим статистику в консоль
        IO.puts(line)
        line
      end
      |> Enum.join("\n")

    # Сохраняем в файл
    File.write!(output_file, stats_content) # Сохраняем статистику в файл
    IO.puts("Статистика сохранена в #{output_file}") # Выводим сообщение в консоль
  end
end

Simulation.run() # Запускаем симуляцию
