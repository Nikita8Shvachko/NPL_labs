(* Конфигурация *)
let input_file = "input.txt"
let output_file = "output.txt"

(* Функция чтения файла целиком *)
let read_file filename = (* читаем файл *)
  let ch = open_in filename in (* открываем файл *)
  let s = really_input_string ch (in_channel_length ch) in (* читаем файл *)
  close_in ch; (* закрываем файл *)
  s (* возвращаем содержимое файла *)

(* Функция записи в файл *)
let write_report filename stats = (* записываем в файл *)
  let ch = open_out filename in (* открываем файл *)
  (* Сортируем статистику по убыванию количества *)
  let sorted_stats = 
    List.sort (fun (_, c1) (_, c2) -> compare c2 c1) (Hashtbl.fold (fun k v acc -> (k, v) :: acc) stats []) (* сортируем статистику по убыванию количества *)
  in (* сортируем статистику по убыванию количества *)
  List.iter (fun (domain, count) -> (* записываем статистику в файл *)
    Printf.fprintf ch "%s: %d\n" domain count (* записываем статистику в файл *)
  ) sorted_stats; (* записываем статистику в файл *)
  close_out ch (* закрываем файл *)

(* Основная логика *)
let () =
  try
    Printf.printf "Чтение файла %s...\n" input_file; (* выводим сообщение о том, что читаем файл *)
    let content = read_file input_file in (* читаем файл *)
    
    (* Регулярное выражение для email *)
    (* [a-zA-Z0-9._%+-]+ - имя пользователя *)
    (* @ - собачка *)
    (* [a-zA-Z0-9.-]+ - домен (упрощенно) *)
    (* \\. - точка перед зоной *)
    (* [a-zA-Z]{2,} - доменная зона (минимум 2 буквы) *)
    let email_regex = Str.regexp "[a-zA-Z0-9._%+-]+@\\([a-zA-Z0-9.-]+\\.[a-zA-Z]+\\)" in (* регулярное выражение для email *)
    
    (* Хеш-таблица для статистики доменов *)
    let stats = Hashtbl.create 10 in (* создаем хеш-таблицу для статистики доменов *)
    let start_pos = ref 0 in (* начальная позиция для поиска *)
    
    Printf.printf "Поиск адресов...\n"; (* выводим сообщение о том, что ищем адреса *)
    
    (* Итеративный поиск всех совпадений *)
    begin
      try
        while true do
          (* Ищем следующее совпадение начиная с start_pos *)
          let _ = Str.search_forward email_regex content !start_pos in
          
          (* Извлекаем найденный email и группу 1 (домен) *)
          let _email = Str.matched_string content in
          let domain = Str.matched_group 1 content in
          
          (* Приводим домен к нижнему регистру для корректного подсчета *)
          let clean_domain = String.lowercase_ascii domain in
          
          (* Обновляем статистику *)
          let count = try Hashtbl.find stats clean_domain with Not_found -> 0 in
          Hashtbl.replace stats clean_domain (count + 1);
          
          (* Сдвигаем позицию поиска *)
          start_pos := Str.match_end ()
        done
      with Not_found -> ()
    end;
    
    Printf.printf "Найдено уникальных доменов: %d\n" (Hashtbl.length stats);
    
    write_report output_file stats;
    Printf.printf "Отчет сохранен в %s\n" output_file
    
  with Sys_error msg ->
    Printf.eprintf "Ошибка: %s\n" msg
