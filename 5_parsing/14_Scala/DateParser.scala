import scala.io.Source
import java.io.{File, PrintWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.{Try, Success, Failure}
import scala.util.matching.Regex

object DateParser {
  // Конфигурация
  val inputFile = "input.txt" // путь к входному файлу
  val outputFile = "output.txt" // путь к выходному файлу

  // Регулярные выражения
  // 1. YYYY-MM-DD: (\d{4})-(\d{2})-(\d{2})
  // 2. DD-MM-YYYY: (\d{2})-(\d{2})-(\d{4})
  // Объединяем через OR (|).
  val datePattern: Regex = raw"(\d{4})-(\d{2})-(\d{2})|(\d{2})-(\d{2})-(\d{4})".r

  def main(args: Array[String]): Unit = {
    println(s"Чтение файла $inputFile...") // выводим сообщение о том, что читаем файл

    if (!new File(inputFile).exists()) {
      println(s"Ошибка: Файл $inputFile не найден.") // выводим сообщение о том, что файл не найден
      return
    }

    val content = Source.fromFile(inputFile).mkString // читаем файл

    // Поиск всех дат
    val foundDates = datePattern.findAllIn(content).matchData.flatMap { m => // ищем все даты и преобразуем в список
      // Пробуем создать дату, если она валидна
      Try { // пытаемся создать дату, если она валидна
        if (m.group(1) != null) {
          // Формат YYYY-MM-DD
          LocalDate.of(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt) // создаем дату
        } else {
          // Формат DD-MM-YYYY
          LocalDate.of(m.group(6).toInt, m.group(5).toInt, m.group(4).toInt) // создаем дату
        }
      }.toOption // Преобразуем Try[LocalDate] в Option[LocalDate] (Failure превратится в None и отфильтруется flatMap'ом)
    }.toList // Превращаем итератор в список

    println(s"Всего найдено дат (с дубликатами): ${foundDates.length}") // выводим количество найденных дат

    // Уникальность и сортировка
    val sortedUniqueDates = foundDates.distinct.sortBy(_.toEpochDay) // сортируем даты

    println(s"Уникальных дат: ${sortedUniqueDates.length}") // выводим количество уникальных дат

    // Запись результата
    val writer = new PrintWriter(new File(outputFile)) // создаем writer для записи результата
    try {
      writer.write("Хронология событий:\n") // записываем хронологию событий
      sortedUniqueDates.foreach { date => // записываем даты
        // Форматируем обратно в ISO-8601 (YYYY-MM-DD)
        writer.write(s"${date.toString}\n") // записываем дату
        println(s"Записано: ${date.toString}") // выводим сообщение о том, что дата записана
      }
      println(s"Результат сохранен в $outputFile") // выводим сообщение о том, что результат сохранен в файл
    } finally {
      writer.close() // закрываем writer
    }
  }
}

