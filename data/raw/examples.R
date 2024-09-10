.libPaths()
library("dplyr")
install.packages("stringr")
library("stringr")

library("readr")
library("psych")
library("tibble")
install.packages("psych")
installed.packages("readr")
old.packages()
qwerty  = read_tsv("data_tsv.tsv", skip = 0, n_max = Inf)
d1 = read_tsv("/home/anton/Downloads/data_tsv.tsv")
d1 = read_rds("/home/anton/Downloads/numeric_data.rds")
d2 = read_csv2("/home/anton/Downloads/data_csv2.csv", skip = 0, n_max = Inf)
write.xlsx(d1, "data/raw/data_excel.xlsx", colNames = TRUE)


write_csv(data, "data/raw/data_csv.csv")

write_excel_csv(data, "data/raw/data_csv.csv")

write_csv2(data, "data/raw/data_csv2.csv")

write_excel_csv2(data, "data/raw/data_csv2.csv")

v <- list(v1 = c(1, -1, 5, -12, -12, 3, 8, -10, 0),
v2 = c(-13, 19, -24, NA, 30, 64, -53, NA, 50, 31, -58, -34, -3, -34, 77),
v3 = c(NA, NA, NA, NA, NA, NA, 3, NA, NA),
v4 = c(-2, 16, -3, 16, -9, 7, 31),
v5 = c(76, 65, 71, 16, 60, 29, 71, 46, 45, 41),
v6 = c(-19, -9, 19, 5, -14, 0, 34, -8, 34, 24, -11, 8, 33, 12, -6)
)
means <- lapply(v, function(x) mean(x, na.rm = TRUE, trim = 0))

v <- list(v1 = c(19, 89, 78, 38, 8, 17, 25, 60, 8, 43, 29, 6, 62, 41, 69, 97, 61, 83, 25, 24),
v2 = c(1, 9, NA, 88, 2, NA, 42, NA, 4, 68, NA),
v3 = c(-92, -50, 54, 55, 84, 52, -55, -23, 36, -11, 22, 11, -7),
v4 = c(-91, -33, 13, 34, 34, 75, -80, -35, -90, -72, 70, 67, -100, -94, -18),
v5 = c(-15, 71, 77, 36, 66, -21, -48, -8)
)
medians <- lapply(v, function(x) median(x, na.rm = TRUE))


v <- list(v1 = c(80.94, 44.46, 46.33, 65.1, 66.42, 104.43, 53.15, 48.41, 12.88, 51.1, 43.03, 40.3, 33.71, 55.1, 22.17),
          v2 = c(26.17, 97.73, 24.81, 53.62, 87.72, 45.19, 45.7, 69.63, 36.76, 7.17),
          v3 = c(63.92, 35.85, 26.9, 48.92, 43.1, 66.94, 47.06, 56.54, 29.1, 58.88),
          v4 = c(32.05, 93.85, 85.52, 56.69, 23.69, 11.29, 51.44, 63.09, 65.65, 35.73, 60.15, 30.93, -4.2)
)
q <- lapply(v, function(x) quantile(x, na.rm = TRUE))

?sd

v <- list(v1 = c(47.44, 62.44, 20.44, 72.75, 77.86, 13.74, 28.2, 50.47, 59.19, 69.04),
v2 = c(49.31, 44.47, 14.04, 44.43, 49.18, 40.73, 44.65, 41.91, 80.38, 80.09),
v3 = c(57.96, 20.81, 8.92, 14.03, 61.02, 25.69, 21.22, 49.56, 25.64, 28.31),
v4 = c(76.22, 65, 19.69, 29.84, 37.18, 70.93, 64.78, 61.66, 49.03, 51.56),
v5 = c(92.11, 56, 47.89, 62.96, 47.41, 37.05, 73.96, 53, 52.37, 85.23)
)
stdev <- lapply(v, function(x) sd(x, na.rm = TRUE))
dispers <- lapply(v, function(x) var(x, na.rm = TRUE))
print(dispers, stdev)

v <- list(v1 = c(47.44, 62.44, 20.44, 72.75, 77.86, 13.74, 28.2, 50.47, 59.19, 69.04),
          v2 = c(49.31, 44.47, 14.04, 44.43, 49.18, 40.73, 44.65, 41.91, 80.38, 80.09),
          v3 = c(57.96, 20.81, 8.92, 14.03, 61.02, 25.69, 21.22, 49.56, 25.64, 28.31),
          v4 = c(57.96, 20.81, 8.92, 14.03, 61.02, 25.69, 21.22, 49.56, 25.64, 28.31),
          v5 = c(92.11, 56, 47.89, 62.96, 47.41, 37.05, 73.96, 53, 52.37, 85.23)
)
err <- lapply(v, function(x) sd(x)/sqrt(length(x)))
print(err)
v <- list(v1 = 
            c(80.94, 44.46, 46.33, 65.1, 66.42, 104.43, 53.15, 48.41, 12.88, 51.1, 43.03, 40.3, 33.71, 55.1, 22.17)
          ,
          v2 = c(26.17, 97.73, 24.81, 53.62, 87.72, 45.19, 45.7, 69.63, 36.76, 7.17),
          v3 = c(63.92, 35.85, 26.9, 48.92, 43.1, 66.94, 47.06, 56.54, 29.1, 58.88),
          v4 = c(32.05, 93.85, 85.52, 56.69, 23.69, 11.29, 51.44, 63.09, 65.65, 35.73, 60.15, 30.93, -4.2)
)
iqar <- lapply(v, function(x) IQR(x, na.rm = FALSE, type = 7))

print(iqar)

sum(!is.na(vec)): количество значений без учёта пропущенных;
sum(is.na(vec)): количество пропущенных значений.
summary(d1)
describe(d1)
?describe

t <- tibble(var_first = 1:10, var_second = ifelse(var_first < 5, var_first + 100, var_first))
View(d1)



data_1 <- tibble(var_1 = 1:8) %>% mutate(id = row_number())

data_2 <- tibble(var_2 = rnorm(10)) %>% mutate(`Subject ID` = row_number())


right_join() #включает все строки, которые есть в y, отбрасывая те, которых там нет
inner_join() #включает все строки, которые есть и в x и y
full_join()  #просто включает все строки, которые есть хотя бы в x или y
left_join() #включает все строки, которые есть в x, отбрасывая из y те, которых нет
group_by()

data <- as.tibble(d1)



data %>% select(`Группа крови`, `Возраст`, function(x) anyNA(x))
data %>% select(function(x) any(str_detect(x, "Жен")))
data %>% select(`Группа`, (function(x) is.factor(x)) | where(function(x) is.numeric(x)))
data %>% select(`Пол`, `Группа`, `Базофилы_E1`)
data %>% select(function(x) sd(x, na.rm = TRUE) > 1)
data %>% select(where(function(x) is.numeric(x) & sd(x, na.rm = TRUE) > 1))
data %>% select(where(function(x) is.numeric(x) & mean(x, na.rm = TRUE) > 3.5))
data %>% select(where(is.numeric) & where(function(x) sd(x, na.rm = TRUE) > 1))
)



data %>% select(where(is.numeric) & where(function(x) sd(x, na.rm = TRUE) > 1))
data %>% select(Группа крови, `Возраст`, function(x) anyNA(x))
data %>% select(function(x) any(str_detect(x, "Жен")))
data %>% select(`Группа крови`, `Возраст`, function(x) anyNA(x))
data %>% select(`Пол`, `Группа`, `Базофилы_E1`)
data %>% select(`Группа`, function(x) !is.factor(x))
data %>% select(`Группа`, (function(x) is.factor(x)) | where(function(x) is.numeric(x)))
data %>% select(function(x) sd(x, na.rm = TRUE) > 1) 


data %>% select(`Группа`, function(x) !is.factor(x))
data %>% select(function(x) any(str_detect(x, "Жен")))
data %>% select(`Пол`, `Группа`, `Базофилы_E1`)
data %>% select(function(x) sd(x, na.rm = TRUE) > 1)
data %>% select(`Группа`, (function(x) is.factor(x)) | where(function(x) is.numeric(x)))
data %>% select(Группа крови, `Возраст`, function(x) anyNA(x))
data %>% select(where(is.numeric) & where(function(x) sd(x, na.rm = TRUE) > 1))
data %>% select(`Группа крови`, `Возраст`, function(x) anyNA(x)) 


data %>% mutate(across(function(x) any(near(x, 0.5, tol = 0.1)), function(x) x + 1000))
data %>% mutate(across(!contains("E1"), function(x) str_c(x, " + некая строка")))
data %>% mutate(across(!contains("E1") & !c(Группа, Возраст) & !where(is.factor), function(x) x ^ 2), across(contains("E2"), function(x) x * 100))
data %>% mutate(across(!contains("E1") & !c(Группа, Возраст), function(x) x ^ 2)) 

