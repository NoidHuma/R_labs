library(readxl)

data_table <- read_excel("uk_soccer.xlsx")

data_p <- data_table[, -c(1,2,11,20)]
data_p_matrix <- as.matrix(data_p)
rownames(data_p_matrix) <- data_table$Олимпиада 


data_p_men <- data_p_matrix[, c(1:8)]
data_p_women <- data_p_matrix[, c(9:16)]
data_p_combined <- data_p_men + data_p_women 
colnames(data_p_combined) <- c("First", "Second", "Third", "4", "5", "6", "7", "8")


# Столбчатая диаграмма по количеству мест
par(mar = c(11, 4, 4, 5), xpd = TRUE)

bp <- barplot(t(data_p_combined), beside = TRUE, col = rainbow(8),
              main = "Количество мест 1-8 по каждой Олимпиаде",
              ylab = "Количество мест",
              las = 2,
              axisnames = TRUE)

last_x <- max(bp)
legend(x = last_x, y = par("usr")[4],
       legend = paste("Место", 1:8),
       fill = rainbow(8),
       bty = "n",
       cex = 0.95,
       seg.len = 0.6,
       x.intersp = 0.3,
       y.intersp = 0.6)

# Значения по умолчанию
par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0), 
    mai = c(1.02, 0.82, 0.82, 0.42), fig = c(0, 1, 0, 1))


# Круговая диаграмм по количеству первых мест
data_only_first <- data_p_combined[, 1]
data_without_zeros <- data_only_first[data_only_first != 0]

colors <- rainbow(length(data_without_zeros))

pie(data_without_zeros, 
    main = "Количество первых мест Великобритании по футболу",
    col = colors,
    labels = data_without_zeros,
    cex = 1.2)

legend("topright",
       legend = names(data_without_zeros),
       fill = colors,      
       title = "Олимпиады",
       cex = 0.95,
       bty = "n")


# тенденции призовых мест по полам
# функциональный
male_trends <- data_table[, c(2)]
female_trends <- data_table[, c(11)]
years_trends <- sapply(data_table[, 1], function(x) as.numeric(gsub("[^0-9]", "", x)))
years_trends <- as.numeric(years_trends)

plot(years_trends, male_trends$`Призовые мужчины`, type = "o", col = "blue", 
     main = "Тенденции изменения количества призовых мест Великобритании по футболу",
     xlab = "Год", ylab = "Количество призовых мест", ylim = c(0, 2), xaxt = "n")
lines(years_trends, female_trends$`Призовые женщины`, type = "o", col = "red")
legend("topright", legend = c("Мужчины", "Женщины"), col = c("blue", "red"), lty = 1)
axis(1, at = years_trends, labels = years_trends, las = 2)

# столбчатый
par(mfrow=c(1,2))
par(mar = c(11, 4, 4, 5))
barplot(t(data_p_men[,c(1,2,3)]), beside = TRUE, col = rainbow(3), 
        main = "Количество мест 1-3 по каждой Олимпиаде (Мужчины)", 
        ylab = "Количество мест",
        las = 2,
        ylim = c(0, 2))

barplot(t(data_p_women[,c(1,2,3)]), beside = TRUE, col = rainbow(3), 
        main = "Количество мест 1-3 по каждой Олимпиаде (Женщины)", 
        ylab = "Количество мест", 
        legend.text = paste("Место", 1:3), args.legend = list(x = "topright",
                                                              cex = 0.95,
                                                              seg.len = 0.6,
                                                              x.intersp = 0.3,
                                                              y.intersp = 0.6
                                                              ),
        las = 2,
        ylim = c(0, 2))

# Значения по умолчанию
par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0), 
    mai = c(1.02, 0.82, 0.82, 0.42), fig = c(0, 1, 0, 1))



# Первые и призовые места по 7 странам и всем видам спорта
data_table_all <- read_excel("olympic.xlsx")

years <- data_table_all$Олимпиада

colors <- c("red", "blue", "green", "orange", "purple", "brown", "pink")

gold_medals <- data_table_all[, c(
  "США 1 место", 
  "Россия 1 место", 
  "Китай 1 место", 
  "Австралия 1 место", 
  "Япония 1 место", 
  "Франция 1 место", 
  "Великобритания 1 место"
)] 

plot(years, gold_medals[[1]], type = "o", col = "red",
     main = "Золотые медали по странам",
     xlab = "",  
     ylab = "Количество золотых медалей",
     ylim = c(0, max(gold_medals, na.rm = TRUE) + 5),
     pch = 19, las = 1)

for (i in 1:7) {
  lines(years, gold_medals[[i]], type = "o", col = colors[i], pch = 19)
}

legend(x = "topleft", legend=c("США", "Россия", "Китай", "Австралия", "Япония", "Франция", "Великобритания"),  
       fill = colors
)

mtext("Год проведения Олимпиады", side = 1, line = 2.5)


total_medals <- data.frame(
  "США" = rowSums(data_table_all[, c("США 1 место", "США 2 место", "США 3 место")]),
  "Россия" = rowSums(data_table_all[, c("Россия 1 место", "Россия 2 место", "Россия 3 место")]),
  "Китай" = rowSums(data_table_all[, c("Китай 1 место", "Китай 2 место", "Китай 3 место")]),
  "Австралия" = rowSums(data_table_all[, c("Австралия 1 место", "Австралия 2 место", "Австралия 3 место")]),
  "Япония" = rowSums(data_table_all[, c("Япония 1 место", "Япония 2 место", "Япония 3 место")]),
  "Франция" = rowSums(data_table_all[, c("Франция 1 место", "Франция 2 место", "Франция 3 место")]),
  "Великобритания" = rowSums(data_table_all[, c("Великобритания 1 место", "Великобритания 2 место", "Великобритания 3 место")])
)

plot(years, total_medals[[1]], type = "o", col = colors[1],
     main = "Общее количество призовых мест (1-3) по странам",
     xlab = "",
     ylab = "Количество призовых мест",
     ylim = c(0, max(total_medals, na.rm = TRUE) + 5),
     pch = 19, las = 1)

for (i in 2:7) {
  lines(years, total_medals[[i]], type = "o", col = colors[i], pch = 19)
}

legend(x = "topleft", legend=c("США", "Россия", "Китай", "Австралия", "Япония", "Франция", "Великобритания"),  
       fill = colors
)

mtext("Год проведения Олимпиады", side = 1, line = 2.5)