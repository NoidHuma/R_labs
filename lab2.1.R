library(readxl)

# Загрузка таблицы из файла
answers <- read_excel("rfiles/lab2_answers.xlsx")
# print(answers)

# Преобразование данных
answers_norm_names <- answers
answers_norm_names[, -1] <- answers[, -1] / 10
answers_norm = answers_norm_names[, -1]

# print(answers_norm_names)


# Пункты 1, 2
result_df <- data.frame(
  Максимальная_оценка = apply(answers_norm, 2, max),  # Максимальная оценка
  Минимальная_оценка = apply(answers_norm, 2, min),   # Минимальная оценка
  Средняя_оценка = apply(answers_norm, 2, mean),      # Средняя оценка
  Любят_жанр = colSums(answers_norm > 0.7),
  Не_любят_жанр = colSums(answers_norm < 0.3)
)

# print(result_df)


# рейтинг
rating_df <- data.frame(
  Жанр = colnames(answers_norm),
  Средняя_оценка = apply(answers_norm, 2, mean)
)

rating_df <- rating_df[order(-rating_df$Средняя_оценка), ]
row.names(rating_df) <- NULL

# print(rating_df)


# выборки строк
# кому нравится и классический рок, и метал
rock_and_metal_lovers <- answers_norm_names[answers_norm_names$"Классический рок" > 0.7 & answers_norm_names$"Метал" > 0.7, ]
print(rock_and_metal_lovers)

# кто поставил хотя бы одну максимальную оценку
max_counts <- rowSums(answers_norm_names[, -1] == 1.0)
max_raters <- answers_norm_names[max_counts > 0, ]
remove(max_counts)


# диаграмма
barplot(
  height = rating_df$Средняя_оценка,  # Высота столбцов (средние оценки)
  names.arg = rating_df$Жанр,         # Названия жанров (подписи по оси X)
  col = "skyblue",                    # Цвет столбцов
  main = "Средняя оценка по жанрам",  # Заголовок диаграммы
  xlab = "Жанр",                      # Подпись оси X
  ylab = "Средняя оценка",            # Подпись оси Y
  ylim = c(0, 1)                      # Ограничение оси Y от 0 до 1
)

all_values <- unlist(answers_norm)

mean_value <- mean(all_values, na.rm = TRUE)

abline(h = mean_value, col = "red", lwd = 2)
