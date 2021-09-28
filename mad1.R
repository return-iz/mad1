
data <- read.table(file      = "data.csv",
                   header    = TRUE,
                   sep       = ";",
                   row.names = 1)

names(data) <- c("Группа",
                 "Пол",
                 "Возраст",
                 "Стаж",
                 "Процент успеха",
                 "Количество ошибок",
                 "Оценка заказчика",
                 "Удовлетворенность заказчика",
                 "Качество документации")

index <- data$`Удовлетворенность заказчика` == "низкая"
data[index, "Удовлетворенность заказчика"] <- as.numeric(1)
index <- data$`Удовлетворенность заказчика` == "средняя"
data[index, "Удовлетворенность заказчика"] <- as.numeric(2)
index <- data$`Удовлетворенность заказчика` == "высокая"
data[index, "Удовлетворенность заказчика"] <- as.numeric(3)
data[, "Удовлетворенность заказчика"] <- as.numeric(data[, "Удовлетворенность заказчика"])

juniors <- subset(data, data$Группа == 1)
seniors <- subset(data, data$Группа == 2)

get_stat <- function(x) {
  x_mean   <- mean(x)
  x_median <- median(x)
  x_sd     <- sd(x)
  x_var    <- var(x)
  x_mad    <- mad(x)
  x_sum    <- sum(x)
  x_diff   <- diff(x)
  x_min    <- min(x)
  x_max    <- max(x)
  x_q1     <- quantile(x, c(0.25, 0.75))
  x_uniq   <- unique(x)
  x_mode   <- x_uniq[which.max(tabulate(match(x, x_uniq)))]
  return(c("Среднее"                       = x_mean,
           "Медиана"                       = x_median,
           "Стандартное отклонение"        = x_sd,
           "Дисперсия"                     = x_var,
           "Абсолютное отклонение медианы" = x_mad,
           "Сумма"                         = x_sum,
           "Минимальное значение"          = x_min,
           "Максимальное значение"         = x_max,
           "Квартили"                      = x_q1,
           "Мода"                          = x_mode))
}

#	мода;
# ассиметрия;
# эксцесс;
# первая квартиль;
# третья квартиль.


##########################
#  Анализ первой группы  #
##########################
View(get_stat(juniors$Пол))
View(get_stat(juniors$Возраст))
View(get_stat(juniors$Стаж))
View(get_stat(juniors$`Процент успеха`))
View(get_stat(juniors$`Количество ошибок`))
View(get_stat(juniors$`Оценка заказчика`))
View(get_stat(juniors$`Качество документации`))


##########################
#  Анализ второй группы  #
##########################
View(get_stat(seniors$Пол))
View(get_stat(seniors$Возраст))
View(get_stat(seniors$Стаж))
View(get_stat(seniors$`Процент успеха`))
View(get_stat(seniors$`Количество ошибок`))
View(get_stat(seniors$`Оценка заказчика`))
View(get_stat(seniors$`Качество документации`))


#########################
#  Анализ всей выборки  #
#########################
View(get_stat(data$Пол))
View(get_stat(data$Возраст))
View(get_stat(data$Стаж))
View(get_stat(data$`Процент успеха`))
View(get_stat(data$`Количество ошибок`))
View(get_stat(data$`Оценка заказчика`))
View(get_stat(data$`Качество документации`))


################################
#  Анализ качественных данных  #
################################
View(get_stat(juniors$`Удовлетворенность заказчика`))
View(get_stat(seniors$`Удовлетворенность заказчика`))
View(get_stat(data$`Удовлетворенность заказчика`))




###############################
#  Графический анализ данных  #
###############################
library(ggpubr)

pairs(~data$Группа                  +
       data$Пол                     +
       data$Возраст                 +
       data$Стаж                    +
       data$`Процент успеха`        +
       data$`Количество ошибок`     +
       data$`Оценка заказчика`      +
       data$`Качество документации`,
      data = data,
      labels = c("Группа",
                 "Пол",
                 "Возраст",
                 "Стаж",
                 "Процент успеха",
                 "Количество ошибок",
                 "Оценка заказчика",
                 "Качество документации"),
      main = "Матричный график")


pie(table(data$`Удовлетворенность заказчика`),
    labels = c("Низкая", "Среднаяя", "Высокая"),
    radius = 1,
    main = "Удовлетворенность заказчика")


pie(table(juniors$`Удовлетворенность заказчика`),
    labels = c("Низкая", "Среднаяя", "Высокая"),
    radius = 1,
    main = "Удовлетворенность заказчика Juniors")
pie(table(seniors$`Удовлетворенность заказчика`),
    labels = c("Низкая", "Среднаяя", "Высокая"),
    radius = 1,
    main = "Удовлетворенность заказчика Seniors")

