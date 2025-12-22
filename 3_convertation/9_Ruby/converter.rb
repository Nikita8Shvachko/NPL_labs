# encoding: utf-8
INPUT_FILE = 'input.md'
OUTPUT_FILE = 'output.html'

def parse_markdown(text)
  html = text.dup

  # заголовки
  # меняем заголовки на html теги через регулярки
  # ^ - начало строки
  # # - решетка
  # (.*) - берем все что дальше в строку
  html.gsub!(/^# (.*)$/, '<h1>\1</h1>')
  html.gsub!(/^## (.*)$/, '<h2>\1</h2>')
  html.gsub!(/^### (.*)$/, '<h3>\1</h3>')
  html.gsub!(/^#### (.*)$/, '<h4>\1</h4>')
  html.gsub!(/^##### (.*)$/, '<h5>\1</h5>')
  html.gsub!(/^###### (.*)$/, '<h6>\1</h6>')

  # жирный текст
  # \*\* - экранируем звездочки
  # .*? - берем символы пока не встретим закрывающие звездочки (в пределах строки)
  html.gsub!(/\*\*([^\n]*?)\*\*/, '<strong>\1</strong>')
  html.gsub!(/__([^\n]*?)__/, '<strong>\1</strong>')

  # курсив
  # то же самое но для одной звездочки или подчеркивания
  # исключаем перенос строки \n чтобы не ломать списки
  html.gsub!(/\*([^*\n]+)\*/, '<em>\1</em>')
  html.gsub!(/_([^_\n]+)_/, '<em>\1</em>')

  # ссылки
  # \[([^\]]+)\] - берем текст ссылки
  # \(([^)]+)\) - берем саму ссылку
  # \1 - текст, \2 - ссылка
  html.gsub!(/\[([^\]]+)\]\(([^)]+)\)/, '<a href="\2">\1</a>')

  # списки
  # нам нужно завернуть элементы списка <li> в общий <ul>
  # идем по строкам и смотрим внутри списка мы или нет
  
  lines = html.split("\n")
  in_list = false # флаг что мы внутри списка
  new_lines = []

  lines.each do |line|
    # если строка начинается с тире или звездочки
    if line.match?(/^(\-|\*) (.*)/)
      unless in_list
        # если еще не в списке - открываем его
        new_lines << "<ul>"
        in_list = true
      end
      # меняем маркер на тег
      new_lines << line.sub(/^(\-|\*) (.*)/, '  <li>\2</li>')
    else
      # если строка не элемент списка
      if in_list
        # но мы были в списке - закрываем его
        new_lines << "</ul>"
        in_list = false
      end
      new_lines << line
    end
  end
  # если файл кончился а список открыт - закрываем
  if in_list
    new_lines << "</ul>"
  end

  html = new_lines.join("\n")

  # параграфы
  # проходим второй раз чтобы найти обычный текст
  # все что не заголовки и не списки - это параграфы
  
  final_lines = []
  html.split("\n").each do |line|
    if line.strip.empty?
      final_lines << "" # пустые строки оставляем
    # пропускаем строки где уже есть html теги
    elsif !line.match?(/^\s*<h/) && !line.match?(/^\s*<ul/) && !line.match?(/^\s*<li/) && !line.match?(/^\s*<\/?ul/)
      final_lines << "<p>#{line}</p>"
    else
      final_lines << line
    end
  end

  final_lines.join("\n")
end

begin
  puts "Начало конвертации Markdown в HTML..."

  unless File.exist?(INPUT_FILE)
    puts "Ошибка: Файл #{INPUT_FILE} не найден."
    exit 1
  end

  content = File.read(INPUT_FILE, encoding: 'UTF-8')
  html_content = parse_markdown(content)

  # заворачиваем в html структуру
  full_html = <<~HTML
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Converted Markdown</title>
</head>
<body>
#{html_content}
</body>
</html>
  HTML

  File.write(OUTPUT_FILE, full_html)

  puts "Успешно! HTML сохранен в #{OUTPUT_FILE}"

rescue StandardError => e
  puts "Произошла ошибка: #{e.message}"
  puts e.backtrace
end
