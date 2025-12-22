-- Функция для чтения всего файла
local function read_file(path) -- читаем файл
    local file = io.open(path, "r")
    if not file then return nil end -- если файл не найден, то возвращаем nil
    local content = file:read("*all")
    file:close() -- закрываем файл
    return content -- возвращаем содержимое файла
end

-- Функция для записи в файл
local function write_file(path, content) -- записываем в файл
    local file = io.open(path, "w")
    if not file then return false end -- если файл не найден, то возвращаем false
    file:write(content) -- записываем содержимое в файл
    file:close() -- закрываем файл
    return true -- возвращаем true
end

local function parse_json(str) -- Парсер JSON (рекурсивный спуск)
    local pos = 1 -- Текущая позиция в строке
    local len = #str -- Длина строки

    -- Функция для просмотра текущего символа без смещения позиции
    local function peek()
        return str:sub(pos, pos)
    end

    -- Функция для получения текущего символа и смещения позиции на 1
    local function consume()
        local char = str:sub(pos, pos)
        pos = pos + 1
        return char
    end

    -- Пропуск пробельных символов
    local function skip_whitespace()
        while pos <= len and str:match("^%s", pos) do
            pos = pos + 1
        end
    end

    -- Генерация ошибки с указанием позиции
    local function error_at(msg)
        error(msg .. " at position " .. pos)
    end

    local parse_value -- Объявление функции парсинга значения (для рекурсии)

    -- Парсинг строки JSON
    local function parse_string()
        consume() -- Пропускаем открывающую кавычку
        local res = ""
        while pos <= len do
            local char = consume()
            if char == '"' then
                return res -- Конец строки
            elseif char == '\\' then -- Обработка экранированных символов
                local next_char = consume()
                if next_char == '"' then res = res .. '"'
                elseif next_char == '\\' then res = res .. '\\'
                elseif next_char == '/' then res = res .. '/'
                elseif next_char == 'b' then res = res .. '\b'
                elseif next_char == 'f' then res = res .. '\f'
                elseif next_char == 'n' then res = res .. '\n'
                elseif next_char == 'r' then res = res .. '\r'
                elseif next_char == 't' then res = res .. '\t'
                else res = res .. next_char
                end
            else
                res = res .. char
            end
        end
        error_at("Unterminated string") -- Ошибка: строка не закрыта
    end

    -- Парсинг числа
    local function parse_number()
        local start = pos
        if str:match("^-", pos) then pos = pos + 1 end -- Знак минус
        while pos <= len and str:match("^%d", pos) do pos = pos + 1 end -- Целая часть
        if str:match("^%.", pos) then -- Дробная часть
            pos = pos + 1
            while pos <= len and str:match("^%d", pos) do pos = pos + 1 end
        end
        if str:match("^[eE]", pos) then -- Экспонента
            pos = pos + 1
            if str:match("^[+-]", pos) then pos = pos + 1 end
            while pos <= len and str:match("^%d", pos) do pos = pos + 1 end
        end
        return tonumber(str:sub(start, pos - 1)) -- Преобразование подстроки в число
    end

    -- Парсинг объекта JSON (таблица в Lua)
    local function parse_object()
        consume() -- Пропускаем открывающую скобку {
        local obj = {}
        skip_whitespace()
        if peek() == "}" then -- Пустой объект
            consume()
            return obj
        end
        
        while true do
            skip_whitespace()
            if peek() ~= '"' then error_at("Expected string key") end
            local key = parse_string() -- Парсим ключ (всегда строка)
            skip_whitespace()
            if consume() ~= ":" then error_at("Expected ':'") end -- Ожидаем двоеточие
            
            obj[key] = parse_value() -- Парсим значение и сохраняем по ключу
            
            skip_whitespace()
            local next_char = consume()
            if next_char == "}" then break -- Конец объекта
            elseif next_char ~= "," then error_at("Expected ',' or '}'") end -- Ожидаем запятую или конец объекта
        end
        return obj
    end

    -- Парсинг массива JSON (список в Lua)
    local function parse_array()
        consume() -- Пропускаем открывающую скобку [
        local arr = {}
        skip_whitespace()
        if peek() == "]" then -- Пустой массив
            consume()
            return arr
        end
        
        while true do
            table.insert(arr, parse_value()) -- Парсим элемент и добавляем в массив
            skip_whitespace()
            local next_char = consume()
            if next_char == "]" then break -- Конец массива
            elseif next_char ~= "," then error_at("Expected ',' or ']'") end -- Ожидаем запятую или конец массива
        end
        return arr
    end

    -- Основная функция парсинга значения (определяет тип по первому символу)
    function parse_value()
        skip_whitespace()
        local char = peek()
        if char == "{" then return parse_object() -- Объект
        elseif char == "[" then return parse_array() -- Массив
        elseif char == '"' then return parse_string() -- Строка
        elseif char:match("[%d%-]") then return parse_number() -- Число (начинается с цифры или минуса)
        elseif str:sub(pos, pos + 3) == "true" then -- Булево true
            pos = pos + 4; return true
        elseif str:sub(pos, pos + 4) == "false" then -- Булево false
            pos = pos + 5; return false
        elseif str:sub(pos, pos + 3) == "null" then -- Null
            pos = pos + 4; return nil
        else
            error_at("Unexpected character: " .. (char or "EOF"))
        end
    end

    return parse_value() -- Запуск парсинга с корневого значения
end


-- Вспомогательная функция для рекурсивной сериализации значения в XML
local function serialize_value(val, indent)
    indent = indent or ""
    if type(val) == "table" then
        local lines = {}
        if #val > 0 then -- Массив
            for _, item in ipairs(val) do
                table.insert(lines, indent .. "<item>" .. serialize_value(item, indent .. "  ") .. "</item>")
            end
        else -- Объект
            for k, v in pairs(val) do
                table.insert(lines, indent .. "<" .. k .. ">" .. serialize_value(v, indent .. "  ") .. "</" .. k .. ">")
            end
        end
        return "\n" .. table.concat(lines, "\n") .. "\n" .. indent
    else
        return tostring(val)
    end
end

-- Основная логика конвертации
local function convert_json_to_xml(input_path, output_path) -- конвертируем JSON в XML
    print("Чтение файла " .. input_path .. "...")
    local json_content = read_file(input_path) -- читаем файл
    
    if not json_content then
        print("Ошибка: Не удалось открыть входной файл.")
        return
    end
    
    print("Парсинг JSON...")
    local status, data = pcall(parse_json, json_content) -- парсим JSON с отловом ошибок
    
    if not status then
        print("Ошибка парсинга JSON: " .. tostring(data))
        return
    end
    
    if type(data) ~= "table" or (next(data) == nil and #data == 0) then
        print("Предупреждение: Данные не найдены или пустой объект.")
    end
    
    print("Генерация XML...")
    local xml_lines = {} -- создаем массив для хранения XML строк
    table.insert(xml_lines, "<employees>") -- добавляем корневой элемент <employees>
    
    -- Обработка как массива объектов (ожидается), так и одиночного объекта
    local list = (#data > 0) and data or {data}
    
    for _, item in ipairs(list) do
        if type(item) == "table" then
            -- Извлекаем ID для атрибута (если есть), остальные поля - теги
            local id = item["id"] -- получаем ID
            local employee_tag = "  <employee"
            
            if id then -- если ID есть, то добавляем его в тег
                employee_tag = employee_tag .. ' id="' .. id .. '"'
            end
            employee_tag = employee_tag .. ">" -- добавляем закрывающий тег >
            table.insert(xml_lines, employee_tag)
            
            for k, v in pairs(item) do -- добавляем остальные поля
                if k ~= "id" then -- если поле не ID, то добавляем его в тег
                    if type(v) == "table" then
                         -- Обработка вложенных структур
                         table.insert(xml_lines, "    <" .. k .. ">" .. serialize_value(v, "      ") .. "</" .. k .. ">")
                    else
                         table.insert(xml_lines, "    <" .. k .. ">" .. tostring(v) .. "</" .. k .. ">")
                    end
                end
            end
            
            table.insert(xml_lines, "  </employee>") -- добавляем закрывающий тег </employee>
        end
    end
    
    table.insert(xml_lines, "</employees>") -- добавляем закрывающий тег </employees>
    
    local xml_content = table.concat(xml_lines, "\n") -- формируем XML строку
    
    if write_file(output_path, xml_content) then -- записываем XML строку в файл
        print("Успешно! Результат сохранен в " .. output_path) -- выводим сообщение о успешном завершении конвертации
    else
        print("Ошибка записи в файл " .. output_path) -- выводим сообщение о ошибке
    end
end

-- Запуск программы
local input_file = arg[1] or "input1.json" -- читаем файл
local output_file = arg[2] or "output.xml" -- пишем файл

convert_json_to_xml(input_file, output_file) -- конвертируем JSON в XML и записываем в файл результат

