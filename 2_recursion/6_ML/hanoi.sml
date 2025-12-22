(* Рекурсивная функция для Ханойской башни *)
(* n: сколько дисков нужно перенести *)


fun hanoi (0, _, _, _) = [] (* Базовый случай: нет дисков, возвращаем пустой список *)
  | hanoi (n, source, aux, dest) =
    hanoi (n - 1, source, dest, aux) @ (* Перемещаем n-1 дисков с source на dest, используя aux как вспомогательный стержень *)
    ["Переместить диск " ^ Int.toString n ^ " со стержня " ^ source ^ " на стержень " ^ dest] @ (* Перемещаем n-ый диск с source на dest *)
    hanoi (n - 1, aux, source, dest) (* Перемещаем n-1 дисков с aux на source, используя dest как вспомогательный стержень *);

(* dest - куда кладем (цель, на которую перемещаем диски) *)
(* aux - чем помогаем (вспомогательный стержень, используемый для перемещения дисков) *)
(* source - откуда берем (исходный стержень, на котором находятся диски) *)

(* Главная часть программы *)
fun main () =
    let
        (* Отсюда читаем *)
        val inputStats = "input.txt" (* читаем *)
        (* Сюда пишем *)
        val outputStats = "output.txt" (* пишем *)

        (* Открываем файл *)
        val ins = TextIO.openIn inputStats (* открываем файл *)
        (* Пытаемся прочитать число *)
        val n_str_opt = TextIO.scanStream (Int.scan StringCvt.DEC) ins (* пытаемся прочитать число *)
        val _ = TextIO.closeIn ins (* закрываем файл *)
        
        val n = case n_str_opt of
                     SOME v => v (* если число прочитано, то сохраняем его *)
                   | NONE => 0 (* Если что-то пошло не так, считаем, что дисков 0 *)
    in
        if n > 0 then (* если число положительное, то запускаем рекурсию *)
            let
                (* Запускаем магию рекурсии *)
                val moves = hanoi (n, "Старт", "Вспом", "Финиш") (* запускаем рекурсию *)
                
                (* Готовимся записывать результат *)
                val outs = TextIO.openOut outputStats (* открываем файл *)
                fun writeList [] = ()
                  | writeList (x::xs) = (TextIO.output (outs, x ^ "\n"); writeList xs) (* записываем результат в файл *)
            in
                writeList moves; (* записываем результат в файл *)
                TextIO.closeOut outs; (* закрываем файл *)
                print ("Успешно сохранили " ^ Int.toString (length moves) ^ " ходов в файл " ^ outputStats ^ "\n")
            end
        else
            print "В файле input.txt должно быть положительное число.\n" (* выводим ошибку *)
    end;

(* Поехали! *)
val _ = main ();
