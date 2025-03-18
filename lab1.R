# ----part 1----
# ----task 1----
p <- 7:4
q <- 0:3

p
q
p + q
p - q
p * q
p / q
p ^ 2
q ^ 2


# ----task 2----
vector1 <- (1:20)
vector1[c(TRUE, FALSE)] <- 0

vector1

vector2 <- 2^(0:19)

vector2

vector3 <- 10^(0:4)

vector3


# ----task 3----
n1 <- 1:50
sum1 <- sum(1 / (n1 * (n1 + 1)))

n2 <- 0:20
sum2 <- sum(1 / 2^n2)

n3 <- 0:9
sequence <- (3 * n3 + 1) / 3^n3
sum3 <- sum(sequence)

count <- sum(sequence > 0.5)

sum1
sum2
sum3
count


# ----task 4----
# Создание вектора vec3
vec3 <- seq(from = 3, to = 27, by = 3)

# Извлечение 2, 5 и 7 значений вектора
vec3[c(2, 5, 7)]

# Извлечение предпоследнего значения вектора
vec3[length(vec3) - 1]

# Извлечение всех значений, кроме предпоследнего
vec3[-(length(vec3) - 1)]

# Возвращение всех элементов вектора, кроме шестого
vec3[-6]

# Попытка извлечь сотое значение вектора (это будет NA, если вектор меньше 100 элементов)
vec3[100]

# Возвращение всех значений вектора vec3, кроме первого и последнего
vec3[-c(1, length(vec3))]

# Нахождение всех значений вектора vec3, которые больше 4, но меньше 10
vec3[vec3 > 4 & vec3 < 10]

# Нахождение всех значений вектора vec3, которые меньше 4 или больше 10
vec3[vec3 < 4 | vec3 > 10]


# ----part 2----
# ----task 1----
country <- rep(c("France", "Italy", "Spain"), each = 5)

year <- rep(2000:2004, times = 3)

country
year


# ----task 2----
weight <- c(1500, 1600, 1550, 1700, 1650)
engine_capacity <- c(1.6, 2.0, 1.6, 2.5, 1.6)
transmission <- c("Manual", "Automatic", "Manual", "Automatic", "Manual")
max_speed <- c(180, 200, 190, 220, 195)
body_type <- c("Sedan", "SUV", "Hatchback", "Sedan", "SUV")

car_brands <- c("Toyota", "Ford", "Honda", "BMW", "Volkswagen")

cars_df <- data.frame(
  Weight = weight,
  Engine_Capacity = engine_capacity,
  Transmission = transmission,
  Max_Speed = max_speed,
  Body_Type = body_type,
  row.names = car_brands
)

filtered_cars <- cars_df[cars_df$Engine_Capacity == 1.6, ]
filtered_cars <- filtered_cars[order(row.names(filtered_cars)), ]

print(cars_df)
print(filtered_cars)

num_rows <- nrow(filtered_cars)
num_cols <- ncol(filtered_cars)

cat("Число строк:", num_rows, "\n")
cat("Число столбцов:", num_cols, "\n")
