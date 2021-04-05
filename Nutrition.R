# Доброе утро, печенеги и половцы. 
# 
# Считайте, что перед вам поварная книга. У вас есть много функций, чтобы ей орудовать: добавлять, изменять,
# удалять и т.д. Пользуйтесь на здоровье. Пока функционал не очень-то большой, но лично по мне этот скрипт удобней
# большинства программ, потому что удобнее и может быть налажен самостоятельно. Жду ваш фидбек.
# 
# Ниже вы увидите некоторый функционал, который поможет вам жрать и не 
# толстеть. Запомните, что, если вы изменяете табли цу в R, то ее нужно будет изменить и в экселе (функция 
# для этого будет представлена ниже). Изменяйте все, пользуясь Эром, а не экселем. Если что пишите,
# я постараюсь дополнить функционал.
# 
# Нажмите сразу alt+0 - зачем вам смотреть на код.

# Путь к папке, в которой будет хранится ваш эксель. Лично у меня он такой
setwd('C:/health') 

loading_packages <- function(){
  packages <- c('readxl', 'tidyverse', 'stringr', 'lubridate')
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) install.packages(setdiff(packages, 
                                                                                              rownames(installed.packages()))) 
  else {library(readxl); library(tidyverse); library(stringr); library(lubridate)}
}

my_cal_per_day <- 2800
my_norm <- c("Завтрак" = 14, "Первое блюдо" = 4, "Из рыбы" = 4, "Мясное блюдо" = 18, "Салат" = 10)
basis <- c('Завтрак', 'Салат')
optionally <- setdiff(names(my_norm), basis)
my_types = c('Завтрак', 'Первое блюдо', "Блюдо из морепродуктов", 
             "Блюдо из птицы или мяса", "Гарнир", "Закуска",
             "Салат", "Перекус", "Десерт", ' ', '...')
norm <- function(norm = my_norm){
  cur_cond <- matrix(0, nrow = 3, ncol = length(norm) + 1)
  cur_cond <- as_tibble(cur_cond, .name_repair = 'minimal')
  names(cur_cond)[1] <- 'Состояние'
  names(cur_cond)[-1] <- names(norm)
  cur_cond[,1, drop = T] <- c('Норма', "На данный момент", "Мультипликатор")
  # Заполнение 1 строки (нормы)
  for (i in 1:length(norm)){
    cur_cond[1, i + 1] = norm[i]
  }
  
  # Заполнение 3 строки (мультипликатора)
  for (i in 1:length(norm)){
    cur_cond[3, i + 1] = max(norm)/norm[i]
  }
  cur_cond <<- cur_cond
}

# Открываем наш файлик. 
open_file <- function(){
  ZOJ <<- read_csv2('Здоровое питание.csv') 
  ZOJ[is.na(ZOJ)] <- ' '
  col <- which(colnames(ZOJ) == "Еда")
  
  
  ZOJ$Тип <- factor(ZOJ$Тип, levels = my_types)
  Products <- ZOJ[, 1:(col - 1)]
  Food <- ZOJ[, col:ncol(ZOJ)]
  print(head(ZOJ, 10))
  ZOJ <<- ZOJ
  Food <<- Food
  Products <<- Products
}
# Пишем в консоли open_file(). Там же должна была отобразиться таблица. Посмотрите, нормально ли она открылась.
# Пару слов о самой таблице.

# Открываем журнал
open_journal <- function(){
  journal <<- read_csv2('Журнал.csv')
  journal[is.na(journal)] <<- ' '
  journal$Time <<- factor(journal$Time, levels = c('Завтрак', "Второй завтрак", 
                                                  "Обед", "Полдник",
                                                  "Ужин", "Второй ужин", ' '), ordered = TRUE)
  
  print(head(journal, 10))
}

# Читаем и выполняем start(). Будет выполнено прочтение библиотек, а также 
# добавление функций во внешнюю среду.
start <- function(){

  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  
is_added <<- function(meal){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  
  if (any(ZOJ == meal)) return(cat('А', meal ,'уже есть в таблице'))
  else return(cat('А вообще', meal, 'отсутствует'))
}

add_meal <<- function(meal, column){
  col_index <- which(colnames(ZOJ) == column)
  
  if (ZOJ[, column, drop = T][1] == '...') ZOJ[1, column] <- meal
  else if (any(ZOJ[, column] == ' ')) {
    row_index <- which(ZOJ[, col_index] == ' ')[1]
    ZOJ[row_index, col_index] = meal 
    return(ZOJ) }
  
  else ZOJ <- rbind(ZOJ, c(rep(' ', col_index - 1), 
                              meal, 
                              rep(' ', ncol(ZOJ) - col_index)))
  
  ZOJ <<- ZOJ
}

delete_meal <<- function(meal){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  ZOJ[ZOJ == meal] = ' '
  ZOJ <<- ZOJ
}

searching <<- function(meal){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  for (i in 1:(ncol(ZOJ) - 1)) {
    if(any(grepl(meal, ZOJ[, i, drop = T], ignore.case = T) == T))
      cat(paste(grep(meal, ZOJ[, i, drop = T], value = T, ignore.case = T), collapse = ', '), 
          'из колонки', colnames(ZOJ)[i], '\n')
  }
}

change_column <<- function(meal, column){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  ZOJ[ZOJ == meal] <<- ' '
  add_meal(meal, column)
}

saving <<- function(dataset_path = "Здоровое питание.csv"){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  
  write_excel_csv2(ZOJ, path = dataset_path, append = FALSE)
  open_file()
}

saving_df <<- function(df1 = Products, df2 = Food){
  ZOJ <<- cbind(df1, df2)
  ZOJ <<- ZOJ %>% 
    as_tibble()
  saving()
}

add_col <<- function(column, number = NULL){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
   
  ZOJ <- ZOJ %>%
  add_column(column = 'Na', .after = number)
  
  if (is.null(number)) colnames(ZOJ)[ncol(ZOJ)] <- column
  else colnames(ZOJ)[number + 1] <- column
  
  col <- which(colnames(ZOJ) == column)
  ZOJ[, col] <- as.character(ZOJ[, col])
  ZOJ[, col, drop = T][-1] <- ' '
  ZOJ[, col, drop = T][1] <- '...'
  
  ZOJ <<- ZOJ
}

delete_col <<- function(column){
  ZOJ[, column] <- NULL
  ZOJ <<- ZOJ
}

add_recipe <<- function(food, composition, period = -1, cal = -1, type = ' ', recipe = ' '){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  
  ZOJ <<- add_meal(food, 'Еда')
  ZOJ <<- add_meal(composition, 'Состав')
  ZOJ$Период[which(ZOJ$Еда == recipe)] <<- period
  ZOJ$Калорийность[which(ZOJ$Еда == recipe)] <<- cal
  ZOJ$Тип[which(ZOJ$Еда == recipe)] <<- type
  ZOJ$Рецепт[which(ZOJ$Еда == recipe)] <<- recipe
  return(ZOJ)
}
  
delete_recipe <<- function(food){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  
  row <- grep(food, ZOJ$Еда, ignore.case = T)
  ZOJ$Еда[row] = ' '
  ZOJ$Состав[row] = ' '
  ZOJ$Период[row] = -1
  ZOJ$Калорийность[row] = -1
  ZOJ$Тип[row] = ' '
  ZOJ$Рецепт[row] = ' '
  ZOJ <<- ZOJ
  return(ZOJ)
}

change_meal <<- function(meal_1, meal_2, column){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  
  selected <- ZOJ %>%
                select(column)
  selected[selected == meal_1] <- meal_2
  ZOJ[, column] <- selected
  return(ZOJ)
}

}

# Основной функционал (на самом деле просто не пихал в start). Также прочитываем и запускаем.
main <- function(){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  
  want_eat_meal <<- function(meal){
    cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
    
    i <- 1
    while (ZOJ$Еда[i] != ' ') {
      unluko <- unlist(str_split(ZOJ[i, "Состав"], ', '))
      if (any(grepl(meal, unluko, ignore.case = T))) cat(unlist(ZOJ[i, "Еда"]), '\n')
      i <- i + 1
    }
  } 
  
  want_eat_dish <<- function(dish){
    cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
    index <- which(ZOJ$Еда == dish)
    return(Food[index,])
  }
  
  # Наименование еды, которую ели давнее всего остального 
  forgotten_dish <<- function(type = ' '){
    if (type == ' ') {
      indexes <- which(Food$Период == max(Food$Период))
      for(i in seq_along(indexes)){
        cat(Food$Еда[indexes[i]], '\n')
      } }
    else {# индексы еды с необходимым типом
      indexes <- which(Food$Тип == type)
      # Название еды с необходимым типом
      selected_food_by_type <- Food[indexes, ]
      # Периоды еды с необходимым типом 
      selected_food <- selected_food_by_type %>%
        filter(Период == max(Период))
      selected_vector <- selected_food[,1, drop = T]
      for (i in seq_along(selected_vector)){
        cat(selected_vector[i], '\n')
      }
      
    }
  }
    
  suggestion <<- function(cal = my_cal_per_day, b = basis,
                          non_b = optionally) {
    
    forgotten <- function(type = ' '){
      if (type == ' ') {
        indexes <- which(Food$Период == max(Food$Период))
        for(i in seq_along(indexes)){
          cat(Food$Еда[indexes[i]], '\n')
        } }
      else {# индексы еды с необходимым типом
        indexes <- which(Food$Тип == type)
        # Название еды с необходимым типом
        selected_food_by_type <- Food[indexes, ]
        # Периоды еды с необходимым типом 
        selected_food <- selected_food_by_type %>%
          filter(Период == max(Период))
        selected_vector <- selected_food[,1, drop = T]
        for (i in seq_along(selected_vector)){
          if (i == length(selected_vector)) return(cat(selected_vector[i]))
          cat(selected_vector[i], ', ')
        }
        
      }
    }
    forgotten1 <- function(type = ' '){
      if (type == ' ') {
        indexes <- which(Food$Период == max(Food$Период))
        for(i in seq_along(indexes)){
          cat(Food$Еда[indexes[i]], '\n')
        } }
      else {# индексы еды с необходимым типом
        indexes <- which(Food$Тип == type)
        # Название еды с необходимым типом
        selected_food_by_type <- Food[indexes, ]
        # Периоды еды с необходимым типом 
        selected_food <- selected_food_by_type %>%
          filter(Период == max(Период))
        selected_vector <- selected_food[,1, drop = T]
        vec <- c()
        for (i in seq_along(selected_vector)){
          vec <- c(vec, selected_vector[i])
        }
        
      }
      return(vec)
    }
    
    multiplied_vector <- cur_cond[2, non_b] * cur_cond[3, non_b]
    filtered_vector <- multiplied_vector[which(multiplied_vector == min(multiplied_vector))]
    needed_types <- colnames(filtered_vector)
    basis_types <- b
    # if (length(c(needed_types, basis_types)) > length(unique(journal$Time))) return(cat
                                                                      # ('В Food нужно добавить еду всех типов, чтобы программа работала корректно'))
    if (is.na(cal)){ 
      needed_types <- c(basis_types, needed_types)
      # калории не имеют значения, важен только период
      for (i in seq_along(needed_types)){
        cat("На", needed_types[i], "можно", '\n')
        cat(forgotten(needed_types[i]), '\n')
      }
    } else {
      # калории имеют значение
     today <- journal %>%
       filter(Date == Sys.Date()) 
     summa <- sum(today$Calories)
     cal = my_cal_per_day - summa
     opt_l <- list()
     basis_l <- list()
     for (i in seq_along(needed_types)){
       result <- forgotten1(needed_types[i]); opt_l[[i]] <- result
     }
     for (i in seq_along(basis_types)){
       result <- forgotten1(basis_types[i]);  basis_l[[i]] <- result
     }
     return(basis_l)
     if (length(l) == 1) {
       # вернуть самое калорийное блюдо
       dishes <- l[[1]]
       indexes <- Food$Еда[which(Food$Еда == dishes)]
       print(indexes)
       calories = Food$Калорийность[which(Food$Калорийность == indexes)]
       print(calories)
     } else {
     for (i in 1:(length(l) - 1)){
       if (i == 1) out = outer(l[[1]], l[[2]])
       else out <- outer(out, l[[i + 1]])
     }
     return(out)
     }
    }
  }

}

main_journal <- function(){
  
  saving_j <<- function(dataset_path = "Журнал.csv"){
    cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
    write_excel_csv2(journal, path = dataset_path, append = FALSE)
    
  }
  
  add_obs <<- function(food, time = ' ', date = Sys.Date(), cal = -1, dataset1 = journal){
    # part 1 (добавляет запись в журнал)
    if (cal == -1){
      if(any(ZOJ$Еда == food)) cal <- ZOJ$Калорийность[which(ZOJ$Еда == food)]
    }
     dataset1 <- dataset1 %>%
                  add_row(Date = as_date(date),
                          Food = food,
                          Time = time,
                          Calories = cal)
     dataset1 <- dataset1[order(dataset1$Date), ]
     dataset1$Time <- factor(dataset1$Time, levels = c('Завтрак', "Второй завтрак", 
                                                      "Обед", "Полдник",
                                                      "Ужин", "Второй ужин", ' '), ordered = TRUE)
     dataset1[dataset1$Date == date, 'Time'] = sort(dataset1[dataset1$Date == date, 'Time', drop = T])
     # part 2 (изменяет датафрейм cur_cond)
     type_index <- which(ZOJ$Еда == food)
     print(type_index)
     type <- ZOJ$Тип[type_index]
     print(type)
     col_index <- which(colnames(cur_cond) == type)
     print(col_index)
     cur_cond[2, col_index, drop = T] <<- cur_cond[2, col_index, drop = T] + 1
     # part 3 (Изменяет колонку Период в ZOJ(ZOJ))
     my_var <<- ZOJ$Период[type_index]
     ZOJ$Период[type_index] <- 0
     type_indexes <- which(ZOJ$Тип == type)
     type_indexes <- type_indexes[which(type_indexes != type_index)]
     ZOJ$Период[type_indexes] <<- ZOJ$Период[type_indexes] + 1
     
     return(dataset1)     
  }
  
  delete_obs <<- function(food, time = NULL, date = Sys.Date(), dataset1 = journal, type_of_food = NULL){
    # part 1
    if (is.null(time)) {
    selected_j <- journal %>%
                    filter(Date == as_date(date), Food == food)
    time = selected_j$Time
    }
    else selected_j <- journal %>%
                              filter(Date == as_date(date), Time == time, Food == food)
    if (nrow(selected_j) == 0) return(cat('Все-таки придется расписать некоторые аргументы вручную. Например, дату'))
    journal <- rows_delete(journal, selected_j, by = c('Date', 'Food', 'Time'))
    
    # part 2
    if (is.null(type_of_food)) {
    type_index <- which(ZOJ$Еда == food)
    
     type <- ZOJ$Тип[type_index]
    } else type <- type_of_food
    col_index <- which(colnames(cur_cond) == type)
    if (length(col_index) == 0) return(cat('Введи, пожалуйста, тип еды (type_of_food) как аргумент, 
                                           чтобы изменить значение в cur_cond'))
    cur_cond[, col_index, drop = T][2] <<- cur_cond[, col_index, drop = T][2] - 1
    if (cur_cond[, col_index, drop = T][2] < 0) cur_cond[, col_index, drop = T][2] <<- 0
    # part 3
    if ( any(as.vector(ls()) == 'my_var') ) {
    ZOJ$Период[type_index] <- my_var
    rm('my_var', envir = .GlobalEnv) }
    type_indexes <- which(ZOJ$Тип == type)
    ZOJ$Период[type_indexes] <<- ZOJ$Период[type_indexes] - 1
    
    return(dataset1)
  }
  
}

main_norm <- function(){
  
  saving_norm <<- function(){
    write_excel_csv(cur_cond, path = 'Двухнедельная норма.csv', append = FALSE)
  }
  
  reset <<- function(){
    cur_cond[2, -1] <- 0
    cur_cond <<- cur_cond
  }
}

# Info() для всех подробностей.
Info <- function(number = 0){
  if (number == 0) cat('Есть ли продукт в таблице (is.added) - наберите Info(1)', '\n', 
      'Добавить продукт в таблицу (add_meal) - наберите Info(2)', '\n',
      'Убрать продукт из таблицы (delete_meal) - наберите Info(3)', '\n',
      'Найти продукт в таблице (searching) - наберите Info(4)', '\n',
      'Изменить колонку для продукта (change_column) - наберите Info(5)', '\n',
      'Сохранить эксель с вашими изменениями (saving) - наберите Info(6)', '\n',
      'Добавить столбец, если нужно (add_col) - наберите Info(7)', '\n', 
      'Добавление блюда (add_recipe) - наберите Info(8)', '\n',
      'Удаление блюда (delete_recipe) - наберите Info(9)', '\n',
      'Замена значений внутри таблицы (change_meal) - наберите Info(10)', '\n', 
      'Во всех функциях есть аргумент по умолчанию, поэтому вам вводить на один аргумент меньше', '\n',
      "Про период: числа напротив блюд показывают, как давно (относительно других блюд) вы готовили именно это блюдо.", '\n',
      '0 - ели совсем недавно, 9 - не ели уже давно. Функции want_eat и choice основаны на этих числах.')
  else if (number == 1) cat('Проверяет, есть ли какой-либо продукт в таблице продуктов.' , '\n', 
              'Потом мб сделаю частичный поиск (по буквам, слогам), но сейчас мне немного лень.')
       if (number == 2) cat('Прямой вывод с прошлой функции: добавляет какой-либо продукт, если он все-таки отсутствует.' , '\n' , 
              'column вставлять с кавычками. Чтобы сохранились изменения в R (!) писать так:', '\n', 
              'dataset <- add_meal(meal, "column", dataset)')
       if (number == 3) cat('Раз есть функция, которая добавляет продукт, то должна быть, которая этот самый продукт убирает.', '\n' ,
                            "Работает по аналогии с add_meal")
       if (number == 4) cat("Ха, наiбав, частичный поиск я все-таки сделал. Можно вбивать буквы, какие-то слоги - все, что угодно.", '\n',
                            'Эта функция должна обесценивать первую функцию, но я не хочу ее удалять, потому что долго писал.')
       if (number == 5) cat('Вот эта штукенция позволяет вам переместить какой-либо продукт из одного отделения (например, белки)', '\n', 
                            'в другое отделение (например, сложные углеводы). Первый аргумент - продукт, второй - куда переместить.')
       if (number == 6) cat('Эта штукенция позволяет сохранить изменения, которые вы делаете, находясь в Р. Если ее не вызывать после', '\n',  
                            'ваших манипуляций (например, после добавления ржаного хлеба в сложные углеводы), то изменения не сохранятся.', '\n', 
                            'Пользуйтесь аккуратно. Еще раз настоятельно советую эксель руками не трогать, всегда лучше через Р.', '\n', 
                            'Работает так: первый аргумент - название датасета в Р, второй - как бы вы хотели назвать файл, который', '\n', 
                            'сохранится на вашем компе. У функции есть аргументы по умолчанию, так что если просто хотите сохранить изменения,', '\n',  
                            'то введите saving().')
       if (number == 7) cat('Добавляете столбец какой хотите куда хотите. Второй аргумент number - таким по счету', '\n',
                            'слева направо будет ваш столбец. Когда вы создаете столбец, то первым значением в нем будет', '\n',
                            '... . Это необходимо ввиду технических нужд. Когда вы будете добавлять блюда или продукты,', '\n', 
                            'многоточие исчезнет. Также по умолчанию аргумент number == NULL, что означает, что если вы не', '\n',
                            'напишите число, то ваш столбец окажется крайним.')
       if (number == 8) cat("Все-таки решил сделать функцию, которая будет добавлять сразу блюдо (еду)", '\n', 
                            "первый аргумент - название блюда, второй - из чего состоит (можете писать туда что угодно в общем-то).", '\n',
                            'Важно #1: Аргументы period и cal (калории) не обязательны для вставки, у них есть значения по умолчанию', '\n',
                            'Важно №2: Состав писать через запятую с пробелом одним символом (в примерах написал как).')
       if (number == 9) cat('Аналогично удаление элемента в Food. Писать нужно полностью и правильно (для этого есть searching).')
       if (number == 10) cat('Последним оператором для работы с таблицей будет change_meal, ответственный за замену values в таблице:', '\n',
                             'meal_1 - что заменить, meal_2 - на что заменить.')
  cat('\n', 'Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
}

# Аналогично.
Info_main <- function(number = 0){
  cat('Кстати говоря', sample(ZOJ$`Аргументы и факты`, 1), '\n')
  if (number == 0) cat ('Найти блюдо по продукту - Info_main(1)', '\n',
                        'Найти состав продуктов для приготовления блюда - Info_main(2)', '\n',
                        "Выбрать блюдо, которое давно не готовил(а) - Info_main(3)", '\n',
                        'Сделать выбор - Info_main(4).')
  if (number == 1) cat('Ищем блюдо по названию. Работает частичный поиск.')
  if (number == 2) cat('Аналогично, только наоборот. Частный поиск не работает, чтобы вам не выдавало по 2 блюда')
  if (number == 3) cat('Дает случайное блюдо, которое вы давно не ели. Ориентируется на период.')
  if (number == 4) cat('Условно делаете выбор. Пока это только ведет к обнулению периода.', '\n',
                       'В дальнейшем мб сделаю что-то посерьезнее, типа выкатывания ингридиентов, рецепта и т.д.', '\n',
                       'Используйте как ZOJ <- choice(...)')
}

# Всего две функции поддерживают частичный поиск: searching и want_eat_meal
# Все аргументы к функциям вводим в кавычки. 

# Examples:
# Можно использовать отдельно
ZOJ <- add_meal('Арбуз', "Фрукты")
ZOJ <- change_column("Рис", "Сложные углеводы")
ZOJ <- delete_meal('Бананы')
saving()
searching("Капуста")
# Можно использовать в комбинации
ZOJ <- add_recipe('Молочная каша',
                  "Молоко, рис, вода",
                  period = 1, cal = 180, type = "Завтрак")
ZOJ <- delete_recipe('Молочная каша')
#
want_eat_random_dish()
ZOJ <- choice('Паста болоньезе')

# Важно понимать, что если вы собираетесь менять саму таблицу (а не получить какое-то сообщение), 
# то понятно, что нужно переприсваивать
ZOJ <- add_meal('Картофель', 'Белки')
