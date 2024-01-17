## 2019-18650 Jeong Yoonhyuk

attract_data <- read.table("attract.dat", header = TRUE, sep = "\t")
satis_data <- read.table("satis.dat", header = TRUE, sep = "\t")

## 10.1 ##
ggplot(attract_data, aes(x = face, y = attract)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.99, color = "blue", fill = "pink", alpha = 0.1) +
  labs(title = "Face - Attract",
       x = "Face",
       y = "Attract")

ggplot(attract_data, aes(x = humor, y = attract)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.99, color = "blue", fill = "pink", alpha = 0.1) +
  labs(title = "Humor - Attract",
       x = "Humor",
       y = "Attract")

ggplot(attract_data, aes(x = charact, y = attract)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.99, color = "blue", fill = "pink", alpha = 0.1) +
  labs(title = "Charact - Attract",
       x = "Charact",
       y = "Attract")

ggplot(attract_data, aes(x = iqscore, y = attract)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.99, color = "blue", fill = "pink", alpha = 0.1) +
  labs(title = "IQscore - Attract",
       x = "IQscore",
       y = "Attract")

## 10.2 ##
ggplot(attract_data, aes(x = attract, y = humor, color = sex)) +
  geom_point() +
  labs(title = "Attract - Humor (Sex)",
       x = "Attractiveness",
       y = "Humor",
       color = "Sex")

## 10.3 ##
ggplot(subset(attract_data, sex == "M"), aes(x = sex, y = iqscore, fill = sex)) +
  geom_boxplot() +
  labs(title = "IQ Score (Males)",
       x = "Sex",
       y = "IQ Score",
       fill = "Sex")

ggplot(subset(attract_data, sex == "F"), aes(x = sex, y = iqscore, fill = sex)) +
  geom_boxplot() +
  labs(title = "IQ Score (Females)",
       x = "Sex",
       y = "IQ Score",
       fill = "Sex")

## 11.1 ##
satis_data$area <- factor(satis_data$area, levels = c(1, 2, 3), labels = c("Seoul", "Province", "Abroad"))

## 11.2 ##
ggplot(satis_data, aes(x = salary)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Salary",
       x = "Salary",
       y = "Frequency")

qqplot <- ggplot(satis_data, aes(sample = salary)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of Salary", y = "Sample Quantiles")

print(qqplot)

## 11.3 ##
ggplot(satis_data, aes(x = log(salary))) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log-Transformed Salary",
       x = "Log(Salary)",
       y = "Frequency")

qqplot_log_salary <- ggplot(satis_data, aes(sample = log(salary))) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of Log-Transformed Salary", y = "Sample Quantiles")
print(qqplot_log_salary)

## 11.4 ##
ggplot(satis_data, aes(x = satis, y = salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue", size = 1) +
  labs(title = "Satis - Salary",
       y = "Salary",
       x = "Satis")

satis_data$log_salary <- log(satis_data$salary)
ggplot(satis_data, aes(x = satis, y = log_salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue", size = 1) +
  labs(title = "Satis - Log(Salary)",
       x = "Satis",
       y = "Log(Salary)")

## 12.1 ##
satis_data2 <- read.table("satis(2).dat", header = TRUE, sep = "\t")
cor_matrix <- cor(satis_data2[, -1])
print(cor_matrix)

## 12.2 ##
dependent_variable <- "satis"
independent_variables <- c("motiv", "envir", "salary", "stress", "area")
reg_model <- lm(formula(paste(dependent_variable, "~", paste(independent_variables, collapse = " + "))), data = satis_data2)
vif_values <- car::vif(reg_model)
print(vif_values)

## 12.3 ##
