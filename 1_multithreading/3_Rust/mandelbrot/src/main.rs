use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::process::Command;
use std::str::FromStr;
use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::time::Instant;

// --- Конфигурация ---

// Структура, описывающая параметры рендеринга и анимации.
// Считывается из input.txt.
#[derive(Debug, Clone)]
struct Config {
    // Ширина кадра
    width: usize,
    // Высота кадра
    height: usize,
    // Максимальное число итераций на самом глубоком зуме
    max_iterations: usize,
    // Количество кадров в анимации
    frames: usize,
    // Количество рабочих потоков
    threads: usize,
    // Коэффициент уменьшения области (зума) за кадр (например, 0.9 = 10% зума)
    zoom_rate: f64,
    // Координата X (Re) точки, в которую мы "влетаем"
    focus_x: f64,
    // Координата Y (Im) точки, в которую мы "влетаем"
    focus_y: f64,
    // Имя выходного файла (используется для базового PPM, но мы делаем GIF)
    output_file: String,
}

impl Config {
    // Загружает настройки из файла.
    fn load(filename: &str) -> io::Result<Self> {
        let file = File::open(filename)?;
        let reader = BufReader::new(file);

        // Значения по умолчанию
        let mut width = 800;
        let mut height = 600;
        let mut max_iterations = 1000;
        let mut frames = 30;
        // Автоматически определяем количество доступных потоков (ядер)
        let mut threads = thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
        let mut zoom_rate = 0.9;
        let mut focus_x = -0.5;
        let mut focus_y = 0.0;
        let mut output_file = "mandelbrot.ppm".to_string();

        // Парсинг конфигурации построчно
        for line in reader.lines() {
            let line = line?;
            // Разбиваем строку по двоеточию
            if let Some((key, value)) = line.split_once(':') {
                let key = key.trim();
                let value = value.trim();
                match key {
                    "width" => width = usize::from_str(value).unwrap_or(width),
                    "height" => height = usize::from_str(value).unwrap_or(height),
                    "max_iterations" => max_iterations = usize::from_str(value).unwrap_or(max_iterations),
                    "frames" => frames = usize::from_str(value).unwrap_or(frames),
                    // "threads" игнорируем, так как определяем автоматически
                    "zoom_rate" => zoom_rate = f64::from_str(value).unwrap_or(zoom_rate),
                    "focus_x" => focus_x = f64::from_str(value).unwrap_or(focus_x),
                    "focus_y" => focus_y = f64::from_str(value).unwrap_or(focus_y),
                    "output_file" => output_file = value.to_string(),
                    _ => {}
                }
            }
        }

        Ok(Config {
            width,
            height,
            max_iterations,
            frames,
            threads,
            zoom_rate,
            focus_x,
            focus_y,
            output_file,
        })
    }
}

// --- Логика Мандельброта ---

// Вычисляет, принадлежит ли точка множеству Мандельброта.
// Возвращает число итераций до "вылета" за пределы радиуса 2.
fn escape_time(c_re: f64, c_im: f64, limit: usize) -> Option<usize> {
    let mut z_re = 0.0;
    let mut z_im = 0.0;
    // Оптимизация: храним квадраты, чтобы не пересчитывать
    let mut z_re_sq = 0.0;
    let mut z_im_sq = 0.0;

    for i in 0..limit {
        z_im = 2.0 * z_re * z_im + c_im;
        z_re = z_re_sq - z_im_sq + c_re;
        z_re_sq = z_re * z_re;
        z_im_sq = z_im * z_im;
        // Если модуль больше 2 (квадрат модуля > 4), точка уходит в бесконечность
        if z_re_sq + z_im_sq > 4.0 {
            return Some(i);
        }
    }
    None
}

// Преобразует число итераций в цвет.
// Использует более сложную палитру для красивого эффекта при зуме.
fn color_pixel(iterations: Option<usize>, max_iterations: usize) -> (u8, u8, u8) {
    match iterations {
        // Черный цвет для внутренней части множества
        None => (0, 0, 0),
        Some(i) => {
            // Нормированное значение итерации [0, 1]
            let t = i as f64 / max_iterations as f64;
            // Палитра на основе полиномов для плавных переходов
            let r = (9.0 * (1.0 - t) * t * t * t * 255.0) as u8;
            let g = (15.0 * (1.0 - t) * (1.0 - t) * t * t * 255.0) as u8;
            let b = (8.5 * (1.0 - t) * (1.0 - t) * (1.0 - t) * t * 255.0) as u8;
            (r, g, b)
        }
    }
}

// --- Логика воркера ---

// Структура задачи для потока.
// Описывает одну строку изображения с конкретными границами (так как границы меняются при зуме).
struct Job {
    // Номер строки (Y)
    row_index: usize,
    // Лимит итераций для текущего кадра
    max_iterations: usize,
    // Границы области для текущего кадра
    xmin: f64,
    xmax: f64,
    ymin: f64,
    ymax: f64,
}

// Результат обработки строки.
struct RowResult {
    // Номер строки
    row_index: usize,
    // Пиксели строки
    data: Vec<(u8, u8, u8)>,
}

// Функция рабочего потока.
// Поток бесконечно берет задачи из канала, пока он не закроется.
fn worker(
    _id: usize,
    config: Arc<Config>,
    job_receiver: Arc<Mutex<mpsc::Receiver<Job>>>,
    result_sender: mpsc::Sender<RowResult>,
) {
    loop {
        // 1. Получение задачи (захват мьютекса для безопасного чтения из канала)
        let job = {
            let receiver = job_receiver.lock().unwrap();
            match receiver.recv() {
                Ok(job) => job,
                // Канал пуст и все отправители удалены -> завершаем работу
                Err(_) => break,
            }
        };

        let y = job.row_index;
        let current_max_iter = job.max_iterations;
        let xmin = job.xmin;
        let xmax = job.xmax;
        let ymin = job.ymin;
        let ymax = job.ymax;
        
        let mut row_data = Vec::with_capacity(config.width);
        
        // 2. Вычисление строки
        // Преобразуем экранные координаты Y в мнимую часть (Im)
        // Используем границы, переданные в Job, так как они зависят от зума
        let img_im = ymax - (y as f64 / config.height as f64) * (ymax - ymin);

        for x in 0..config.width {
            // Преобразуем экранные координаты X в действительную часть (Re)
            let img_re = xmin + (x as f64 / config.width as f64) * (xmax - xmin);
            
            // Считаем количество итераций
            let iterations = escape_time(img_re, img_im, current_max_iter);
            // Превращаем итерации в цвет
            let color = color_pixel(iterations, current_max_iter);
            row_data.push(color);
        }

        // 3. Отправка результата главному потоку
        if result_sender.send(RowResult { row_index: y, data: row_data }).is_err() {
            break;
        }
    }
}

// --- Главная функция ---

fn main() -> io::Result<()> {
    let start_time = Instant::now(); // Время начала генерации

    // 1. Загрузка конфигурации
    let config = Arc::new(Config::load("input.txt").or_else(|_| Config::load("1_multithreading/Rust/mandelbrot/input.txt"))?);
    println!("Configuration loaded: {:?}", config); // Выводим конфигурацию в консоль

    // Подготовка директории для сохранения кадров
    let frames_dir = "frames";
    if fs::metadata(frames_dir).is_ok() {
        // Удаляем старые кадры, чтобы не смешивались
        fs::remove_dir_all(frames_dir)?; // Удаляем старые кадры
    }
    fs::create_dir_all(frames_dir)?;

    // 2. Создание каналов
    // job_channel: Main -> Workers (передает задачи на расчет строк)
    let (job_sender, job_receiver) = mpsc::channel();
    // Mutex нужен, чтобы несколько воркеров читали из одного канала
    let job_receiver = Arc::new(Mutex::new(job_receiver));
    
    // result_channel: Workers -> Main (возвращает готовые пиксели)
    let (result_sender, result_receiver) = mpsc::channel();

    // 3. Запуск пула потоков
    // Потоки живут всю программу и обрабатывают задачи кадр за кадром
    let mut handles = vec![]; // Вектор для хранения потоков
    for i in 0..config.threads {
        let config_ref = Arc::clone(&config); // Клонируем конфигурацию
        let job_rx = Arc::clone(&job_receiver); // Клонируем канал задач
        let res_tx = result_sender.clone(); // Клонируем канал результатов
        
        handles.push(thread::spawn(move || {
            worker(i, config_ref, job_rx, res_tx); // Запускаем воркер
        }));
    }

    // 4. Главный цикл анимации (генерация кадров)
    
    // Канал для отправки готовых кадров на запись (Main -> Writer)
    let (file_sender, file_receiver) = mpsc::channel::<(usize, Vec<Vec<(u8, u8, u8)>>)>();
    let writer_config = Arc::clone(&config);
    let writer_frames_dir = frames_dir.to_string();

    // Запускаем поток записи файлов
    let writer_handle = thread::spawn(move || {
        for (frame_idx, buffer) in file_receiver {
            let filename = format!("{}/frame_{:04}.ppm", writer_frames_dir, frame_idx);
            let file = match File::create(&filename) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("Error creating file {}: {}", filename, e);
                    continue;
                }
            };
            let mut writer = BufWriter::new(file);

            // Пишем заголовок PPM
            // P3 - текстовый формат, ширина, высота, макс. значение цвета
            if let Err(e) = writeln!(writer, "P3\n{} {}\n255", writer_config.width, writer_config.height) {
                 eprintln!("Error writing header for {}: {}", filename, e);
                 continue;
            }

            for row in buffer {
                for (r, g, b) in row {
                    if let Err(_) = write!(writer, "{} {} {} ", r, g, b) {
                        break;
                    }
                }
                if let Err(_) = writeln!(writer) { break; }
            }
        }
    });

    // Начальные границы области (весь фрактал)
    let mut current_xmin = -2.5; // Начальная координата X
    let mut current_xmax = 1.5; // Начальная координата X
    // Y вычисляем исходя из пропорций экрана, чтобы не было искажений
    let mut current_ymin; // Начальная координата Y
    let mut current_ymax; // Начальная координата Y
    
    {
        let aspect_ratio = config.width as f64 / config.height as f64; // Вычисляем пропорции экрана
        let current_width = current_xmax - current_xmin;
        let current_height = current_width / aspect_ratio;
        current_ymin = -current_height / 2.0;
        current_ymax = current_height / 2.0;
    }

    // Удаляем лишний экземпляр result_sender у главного потока, чтобы цикл приема не завис
    drop(result_sender);

    for frame in 0..config.frames {
        // Динамическая детализация:
        // Чем глубже зум, тем больше итераций нужно для четкости границ.
        // Линейно увеличиваем от 100 до max_iterations.
        let iter_progress = frame as f64 / config.frames as f64; // Прогресс зума
        let current_max_iter = (100.0 + (config.max_iterations as f64 - 100.0) * iter_progress) as usize;

        println!("Generating frame {}/{} (iter: {}, range: {:.2e})", 
            frame + 1, config.frames, current_max_iter, current_xmax - current_xmin); // Выводим прогресс в консоль
        
        // 4.1 Раздача задач воркерам (построчно)
        for y in 0..config.height {
            job_sender.send(Job { 
                row_index: y, // Номер строки
                max_iterations: current_max_iter,
                // Передаем текущие границы зума в задачу
                xmin: current_xmin,
                xmax: current_xmax,
                ymin: current_ymin,
                ymax: current_ymax,
            }).unwrap();
        }

        // 4.2 Сбор результатов
        // Используем Option, чтобы инициализировать вектор без аллокации лишних данных
        let mut frame_rows: Vec<Option<Vec<(u8, u8, u8)>>> = vec![None; config.height];
        
        // Ждем ровно столько ответов, сколько строк отправили
        for _ in 0..config.height {
            let result = result_receiver.recv().map_err(|e| io::Error::new(io::ErrorKind::Other, e))?; // Получаем результат
            frame_rows[result.row_index] = Some(result.data);
        }
        
        // Собираем полный кадр
        let final_buffer: Vec<Vec<(u8, u8, u8)>> = frame_rows.into_iter().map(|opt| opt.unwrap()).collect();

        // 4.3 Отправка кадра на запись в отдельный поток
        file_sender.send((frame, final_buffer)).unwrap();

        // 4.4 Расчет зума для следующего кадра
        let width = current_xmax - current_xmin;
        let height = current_ymax - current_ymin;
        
        // Уменьшаем область видимости (эффект приближения)
        let new_width = width * config.zoom_rate;
        let new_height = height * config.zoom_rate;

        // Интерполяция центра:
        // Нам нужно не просто уменьшить картинку, но и сместить центр к точке интереса (focus_x, focus_y) для зума.
        // Текущий центр:
        let current_center_x = current_xmin + width / 2.0;
        let current_center_y = current_ymin + height / 2.0;
        
        // Сдвигаем центр пропорционально зуму, чтобы точка фокуса оставалась на месте
        let move_factor = 1.0 - config.zoom_rate;
        let new_center_x = current_center_x + (config.focus_x - current_center_x) * move_factor;
        let new_center_y = current_center_y + (config.focus_y - current_center_y) * move_factor;

        // Пересчитываем новые границы
        current_xmin = new_center_x - new_width / 2.0;
        current_xmax = new_center_x + new_width / 2.0;
        current_ymin = new_center_y - new_height / 2.0;
        current_ymax = new_center_y + new_height / 2.0;
    }

    // 5. Завершение работы
    // Закрываем канал задач -> воркеры выходят из цикла -> потоки завершаются
    drop(job_sender);
    for handle in handles {
        handle.join().unwrap(); // Ждем завершения потоков
    }
    
    // Закрываем канал записи и ждем завершения писателя
    drop(file_sender);
    writer_handle.join().unwrap();

    println!("Generation finished in {:.2?}", start_time.elapsed()); // Выводим время генерации в консоль

    // 6. Сборка GIF с помощью ffmpeg
    let gif_output = "mandelbrot_zoom.gif"; // Имя выходного файла
    println!("Создание GIF с помощью ffmpeg...");
    
    // Запускаем ffmpeg как внешний процесс
    let status = Command::new("ffmpeg")
        .arg("-y")          // Перезаписываем файл
        .arg("-framerate")
        .arg("10")          // 10 кадров в секунду для GIF
        .arg("-i")
        .arg(format!("{}/frame_%04d.ppm", frames_dir)) // Шаблон имени файлов для PPM
        .arg(gif_output) // Имя выходного файла для GIF
        .status();

    match status {
        Ok(s) if s.success() => println!("GIF created: {}", gif_output),
        _ => eprintln!("FFmpeg failed (make sure it is installed)"),
    }

    Ok(())
}
