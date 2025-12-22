import Foundation

// Конфигурация
let inputPath = "input.txt"
let outputPath = "output.txt"
let baseCurrency = "RUB"

// Курсы валют (фиксированные)
let exchangeRates: [String: Double] = [ // курсы валют (фиксированные для примера)
    "RUB": 1.0,
    "USD": 92.50,
    "EUR": 99.80,
    "JPY": 1.50,
    "GBP": 1.20,
    "CHF": 1.10,
    "CNY": 1.30,
    "INR": 1.40,
    "BRL": 1.60,
    "MXN": 1.70,
    "CAD": 1.80,
    "AUD": 1.90,
    "NZD": 2.00,
    "SGD": 2.10,
    "HKD": 2.20,
    "KRW": 2.30,
    "TRY": 2.40
]

func main() {
    do {
        // 1. Чтение файла
        let fileContent = try String(contentsOfFile: inputPath, encoding: .utf8) // читаем файл и преобразуем в строку
        print("Файл прочитан. Размер: \(fileContent.count) байт.")
        
        // 2. Регулярное выражение
        // Ищем число (целое или дробное) и валюту
        // (\d+(?:\.\d{1,2})?) - группа 1: число (например, 100 или 12.50) 
        // \s+ - пробелы
        // (RUB|USD|EUR|JPY|GBP|CHF|CNY|INR|BRL|MXN|CAD|AUD|NZD|SGD|HKD|KRW|TRY) - группа 2: валюта
        let pattern = #"(\d+(?:\.\d{1,2})?)\s+(RUB|USD|EUR|JPY|GBP|CHF|CNY|INR|BRL|MXN|CAD|AUD|NZD|SGD|HKD|KRW|TRY)\b"#
        let regex = try NSRegularExpression(pattern: pattern, options: []) // создаем регулярное выражение
        
        let range = NSRange(location: 0, length: fileContent.utf16.count) // создаем диапазон для поиска
        let matches = regex.matches(in: fileContent, options: [], range: range) // ищем совпадения
        
        print("Найдено совпадений: \(matches.count)") // выводим количество найденных совпадений
        
        var totalSum: Double = 0.0 // общая сумма
        var logOutput = "Найденные цены:\n"
        
        // 3. Обработка совпадений
        for match in matches { // обрабатываем все совпадения
            // Извлекаем подстроки по диапазонам групп
            if let amountRange = Range(match.range(at: 1), in: fileContent), // извлекаем число
               let currencyRange = Range(match.range(at: 2), in: fileContent) { // извлекаем валюту
                
                let amountStr = String(fileContent[amountRange]) // преобразуем число в строку
                let currencyStr = String(fileContent[currencyRange]) // преобразуем валюту в строку
                
                if let amount = Double(amountStr), // преобразуем число в double
                   let rate = exchangeRates[currencyStr] { // получаем курс валюты
                    
                    let convertedAmount = amount * rate // конвертируем сумму в рубли
                    totalSum += convertedAmount // добавляем конвертированную сумму к общей сумме
                    
                    let line = "- \(amountStr) \(currencyStr) -> \(String(format: "%.2f", convertedAmount)) RUB\n" // формируем строку для вывода
                    logOutput += line
                    print(line.trimmingCharacters(in: .whitespacesAndNewlines)) // выводим строку
                }
            }
        }
        
        // 4. Запись результата
        let resultString = """
        \(logOutput)
        ----------------------------
        ИТОГО: \(String(format: "%.2f", totalSum)) RUB
        """ // формируем строку для вывода
        
        try resultString.write(toFile: outputPath, atomically: true, encoding: .utf8) // записываем результат в файл
        print("Результат сохранен в \(outputPath)") // выводим сообщение о том, что результат сохранен в файл
        
    } catch {
        print("Ошибка: \(error)") // выводим сообщение о ошибке
    }
}

main()

