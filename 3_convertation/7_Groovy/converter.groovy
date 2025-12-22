// Импортируем класс для парсинга XML
import groovy.xml.XmlSlurper
import java.nio.file.Paths

// Определяем пути к файлам
def inputFilePath = 'input.xml'
def outputFilePath = 'output.csv'

try {
    // 1. Чтение входного файла
    def inputFile = new File(inputFilePath)
    if (!inputFile.exists()) {
        println "Ошибка: Входной файл '${inputFilePath}' не найден."
        return
    }

    // Парсим XML файл с помощью XmlSlurper
    // XmlSlurper - это удобный класс в Groovy для чтения XML, который не требует создания DOM-дерева в памяти
    def employees = new XmlSlurper().parse(inputFile) 

    // 2. Подготовка данных
    // Определяем заголовки для CSV
    def headers = ['id', 'name', 'role', 'salary'] 
    
    // Создаем или перезаписываем выходной файл
    def outputFile = new File(outputFilePath)
    
    // Используем буферизированный райтер для эффективной записи
    outputFile.withWriter('UTF-8') { writer ->
        
        // 3. Запись заголовков
        // join(',') объединяет элементы списка в одну строку через запятую
        writer.writeLine(headers.join(','))

        // 4. Обработка каждого сотрудника
        // employees.employee итерируется по всем дочерним тегам <employee>
        employees.employee.each { employee ->
            
            def id = employee.@id.text() // получаем id сотрудника
            def name = employee.name.text() // получаем имя сотрудника
            def role = employee.role.text() // получаем роль сотрудника
            def salary = employee.salary.text() // получаем зарплату сотрудника

            def csvLine = [id, name, role, salary].join(',') // формируем строку CSV
            writer.writeLine(csvLine) // записываем строку CSV в файл
        }
    }

    println "Конвертация успешно завершена. Результат сохранен в '${outputFilePath}'" // выводим сообщение о успешном завершении конвертации

} catch (Exception e) {
    println "Произошла ошибка при конвертации: ${e.message}" // выводим сообщение о ошибке
    e.printStackTrace()
}

