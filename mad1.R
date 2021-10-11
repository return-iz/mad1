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

#index <- data$`Удовлетворенность заказчика` == "низкая"
#data[index, "Удовлетворенность заказчика"] <- as.numeric(1)
#index <- data$`Удовлетворенность заказчика` == "средняя"
#data[index, "Удовлетворенность заказчика"] <- as.numeric(2)
#index <- data$`Удовлетворенность заказчика` == "высокая"
#data[index, "Удовлетворенность заказчика"] <- as.numeric(3)
#data[, "Удовлетворенность заказчика"] <- as.numeric(data[, "Удовлетворенность заказчика"])

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
#View(get_stat(juniors$`Удовлетворенность заказчика`))
#View(get_stat(seniors$`Удовлетворенность заказчика`))
#View(get_stat(data$`Удовлетворенность заказчика`))




###############################
#  Графический анализ данных  #
###############################

plot(data$Стаж, data$`Количество ошибок`, xlab="Стаж", ylab="Количество ошибок")


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


pie(table(subset(juniors, juniors$Пол == 1)$`Удовлетворенность заказчика`),
    labels = c("Низкая", "Среднаяя", "Высокая"),
    radius = 1,
    main = "Удовлетворенность заказчика мужчинами Juniors")

pie(table(subset(seniors, seniors$Пол == 1)$`Удовлетворенность заказчика`),
    labels = c("Низкая", "Среднаяя", "Высокая"),
    radius = 1,
    main = "Удовлетворенность заказчика мужчинами Seniors")

pie(table(subset(juniors, juniors$Пол == 2)$`Удовлетворенность заказчика`),
    labels = c("Низкая", "Среднаяя", "Высокая"),
    radius = 1,
    main = "Удовлетворенность заказчика женщинами Juniors")

pie(table(subset(seniors, seniors$Пол == 2)$`Удовлетворенность заказчика`),
    labels = c("Низкая", "Среднаяя", "Высокая"),
    radius = 1,
    main = "Удовлетворенность заказчика женщинами Seniors")


boxplot(subset(juniors, juniors$Пол == 1, select = "Оценка заказчика"),
        main="Диаграмма размаха для мужчин juniors",
        ylab="Оценка заказчика")

boxplot(subset(seniors, seniors$Пол == 1, select = "Оценка заказчика"),
        main="Диаграмма размаха для мужчин seniors",
        ylab="Оценка заказчика")

boxplot(subset(juniors, juniors$Пол == 2, select = "Оценка заказчика"),
        main="Диаграмма размаха для женщин juniors",
        ylab="Оценка заказчика")

boxplot(subset(seniors, seniors$Пол == 2, select = "Оценка заказчика"),
        main="Диаграмма размаха для женщин seniors",
        ylab="Оценка заказчика")

par(mfrow = c(4, 2))
barplot(table(data$`Пол`), main = "Пол")
barplot(table(data$`Возраст`), main = "Возраст")
barplot(table(data$`Стаж`), main = "Стаж")
barplot(table(data$`Процент успеха`), main = "Процент успеха")
barplot(table(data$`Количество ошибок`), main = "Количество ошибок")
barplot(table(data$`Оценка заказчика`), main = "Оценка заказчика")
barplot(table(data$`Качество документации`), main = "Качество документации")







#################################
#  Кореляционный анализ данных  #
#################################
MJ <- juniors[, unlist(lapply(juniors, is.numeric))][, seq(2,8)]
MS <- seniors[, unlist(lapply(seniors, is.numeric))][, seq(2,8)]

JP <- cor(MJ, use = "all.obs")
JS <- cor(MJ, use = "all.obs", method = "spearman")
JK <- cor(MJ, use = "all.obs", method = "kendall")

SP <- cor(MS, use = "all.obs")
SS <- cor(MS, use = "all.obs", method = "spearman")
SK <- cor(MS, use = "all.obs", method = "kendall")

library(ggm)
pcor(c("Количество ошибок", "Оценка заказчика", 
       "Пол", "Возраст", "Стаж", "Процент успеха", "Качество документации"),
     cov(MJ))
pcor(c("Количество ошибок", "Оценка заказчика", 
       "Пол", "Возраст", "Стаж", "Процент успеха", "Качество документации"), 
     cov(MS))

library(corrplot)
corrplot(JP, 
         method     = "color", 
         type        = "upper", 
         outline     = TRUE, 
         order       = "hclust", 
         tl.srt      = 45,
         tl.col      = "black",
         addCoef.col = "black", 
         diag        = FALSE)
corrplot(JS, 
         method     = "color", 
         type        = "upper", 
         outline     = TRUE, 
         order       = "hclust", 
         tl.srt      = 45,
         tl.col      = "black",
         addCoef.col = "black", 
         diag        = FALSE)
corrplot(JK, 
         method     = "color", 
         type        = "upper", 
         outline     = TRUE, 
         order       = "hclust", 
         tl.srt      = 45,
         tl.col      = "black",
         addCoef.col = "black", 
         diag        = FALSE)

corrplot(SP, 
         method     = "color", 
         type        = "upper", 
         outline     = TRUE, 
         order       = "hclust", 
         tl.srt      = 45,
         tl.col      = "black",
         addCoef.col = "black", 
         diag        = FALSE)
corrplot(SS, 
         method     = "color", 
         type        = "upper", 
         outline     = TRUE, 
         order       = "hclust", 
         tl.srt      = 45,
         tl.col      = "black",
         addCoef.col = "black", 
         diag        = FALSE)
corrplot(SK, 
         method     = "color", 
         type        = "upper", 
         outline     = TRUE, 
         order       = "hclust", 
         tl.srt      = 45,
         tl.col      = "black",
         addCoef.col = "black", 
         diag        = FALSE)
