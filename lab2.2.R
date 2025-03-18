# Импорт данных из CSV-файла
example_csv <- read.csv("rfiles/data.csv")

# Загрузка таблицы из файла
answers <- read_excel("rfiles/lab2_answers.xlsx")
# print(answers)

# Преобразование данных
answers_norm_names <- answers
answers_norm_names[, -1] <- answers[, -1] / 10
answers_norm = answers_norm_names[, -1]


# гистогрммы для всех жанров
par(mfrow = c(3, 4))

for (col in colnames(answers_norm_names[, -1])) {
  hist(answers_norm_names[[col]], 
       main = paste("Распределение оценок для", col), 
       xlab = "Оценка", 
       ylab = "Частота", 
       col = "skyblue", 
       breaks = 10)
}
par(mfrow = c(1, 1))


# боксплот
par(mar = c(12, 4, 4, 2))
boxplot(answers_norm_names[, -1], 
        main = "Боксплот оценок по жанрам", 
        xlab = "Жанр", 
        ylab = "Оценка", 
        col = "lightgreen", 
        las = 2)  # Поворот подписей на 90 градусов
par(mar = c(5, 4, 4, 2))


# серединные меры
middle_answers = summary(answers_norm_names[,-1])
middle_answers


# создание и обработка поднаборов
# поднабор студентов >0.7 классический рок
subdataset_high <- answers_norm_names[answers_norm_names$"Классический рок" > 0.7, ]

# поднабор студентов <0.4 джаз
subdataset_low <- answers_norm_names[answers_norm_names$Джаз < 0.4, ]

# размерности поднаборов
cat("Размерность subdataset_high:", dim(subdataset_high), "\n")
cat("Размерность subdataset_low:", dim(subdataset_low), "\n")


# гистограммы для subdataset_high
par(mfrow = c(3, 4))
for (col in colnames(subdataset_high[, -1])) {
  hist(subdataset_high[[col]], 
       main = paste("Высокие оценки: Распределение для", col), 
       xlab = "Оценка", 
       ylab = "Частота", 
       col = "skyblue", 
       breaks = 10)
}
par(mfrow = c(1, 1))

# гистограммы для subdataset_low
par(mfrow = c(3, 4))
for (col in colnames(subdataset_low[, -1])) {
  hist(subdataset_low[[col]], 
       main = paste("Низкие оценки: Распределение для", col), 
       xlab = "Оценка", 
       ylab = "Частота", 
       col = "lightgreen", 
       breaks = 10)
}
par(mfrow = c(1, 1))


# Боксплот для subdataset_high
par(mar = c(12, 4, 4, 2))
boxplot(subdataset_high[, -1], 
        main = "Боксплот для высоких оценок классического рока (> 0.7)", 
        xlab = "Жанр", 
        ylab = "Оценка", 
        col = "skyblue", 
        las = 2)
par(mar = c(5, 4, 4, 2))

# Боксплот для subdataset_low
par(mar = c(12, 4, 4, 2))
boxplot(subdataset_low[, -1], 
        main = "Боксплот для низких оценок джаза (< 0.4)", 
        xlab = "Жанр", 
        ylab = "Оценка", 
        col = "lightgreen", 
        las = 2)
par(mar = c(5, 4, 4, 2))


# серединные меры для subdataset_high
cat("Серединные меры для subdataset_high:\n")
print(summary(subdataset_high[, -1]))

# серединные меры для subdataset_low
cat("\nСерединные меры для subdataset_low:\n")
print(summary(subdataset_low[, -1]))



# загружаем две таблицы и делаем слияние
answers1 <- read_excel("rfiles/lab2_answers_part1.xlsx")

answers1_norm_names <- answers1
answers1_norm_names[, -1] <- answers1[, -1] / 10
answers1_norm = answers1_norm_names[, -1]


answers2 <- read_excel("rfiles/lab2_answers_part2.xlsx")

answers2_norm_names <- answers2
answers2_norm_names[, -1] <- answers2[, -1] / 10
answers2_norm = answers2_norm_names[, -1]

# Слияние таблиц по столбцу "фамилия"
answers_norm_names_main <- merge(answers1_norm_names, answers2_norm_names, by = "Фамилия")


# загружаем еще таблицу и добавляем ее строки к нашей
answers3 <- read_excel("rfiles/lab2_answers_part3.xlsx")

answers3_norm_names <- answers3
answers3_norm_names[, -1] <- answers3[, -1] / 10
answers3_norm = answers3_norm_names[, -1]

answers_norm_names_main <- rbind(answers_norm_names_main, answers3_norm_names)


# исключение переменных
myvars <- names(answers_norm_names_main) %in% c("Поп", "Джаз") 
new_answers <- answers_norm_names_main[!myvars]
remove(myvars)

new_answers <- answers_norm_names_main[c(-2,-3)]


# subset
new_answers <- subset(answers_norm_names_main, Метал > 0.7 | Рэп < 0.4, select=c(Фамилия, Метал, Рэп)) 





