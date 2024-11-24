AA <- RAW

AA$IsZen <- ifelse(AA$W22DQ02A >= 1995, 1, 0)
head(AA$IsZen)

AA$W22Q27B_역코딩 <- 6 - AA$W22Q27B
AA$W22Q27C_역코딩 <- 6 - AA$W22Q27C
AA$이직의도 <- rowMeans(AA[, c("W22Q27A", "W22Q27B_역코딩", "W22Q27C_역코딩")], na.rm = TRUE)
head(AA$이직의도)

# 역코딩된 값 확인
table(AA$이직의도)


################################################################################

colnames(AA)[colnames(AA) == "W22Q03A"] <- "입사직급"
colnames(AA)[colnames(AA) == "W22Q03B"] <- "현재직급"

# 1. 현재직급이 1인 샘플 필터링
filtered_data <- AA[AA$현재직급 == 1, ]

# 2. 두 세대 간 이직의도 차이를 확인하기 위한 t-검정
t_test_result <- t.test(이직의도 ~ IsZen, data = filtered_data)

# 3. t-검정 결과 출력
print("현재직급이 1인 기성세대와 Z세대의 이직의도 차이에 대한 t-검정 결과:")
print(t_test_result)

# 1. Z세대의 이직의도 히스토그램
ggplot(AA[AA$IsZen == 1, ], aes(x = 이직의도)) +
  geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
  labs(title = "Z세대의 이직의도 분포", x = "이직의도", y = "빈도")

# 2. 기성세대의 이직의도 히스토그램
ggplot(AA[AA$IsZen == 0, ], aes(x = 이직의도)) +
  geom_histogram(binwidth = 0.1, fill = "red", alpha = 0.7) +
  labs(title = "기성세대의 이직의도 분포", x = "이직의도", y = "빈도")


# 1. 데이터 필터링 (IsZen 그룹별 데이터)
gen0_data <- filtered_data$이직의도[filtered_data$IsZen == 0]
gen1_data <- filtered_data$이직의도[filtered_data$IsZen == 1]

# 2. 정규성 검정 (Shapiro-Wilk Test)
shapiro_gen0 <- shapiro.test(gen0_data)
shapiro_gen1 <- shapiro.test(gen1_data)

print("기성세대 (IsZen == 0)의 정규성 검정 결과:")
print(shapiro_gen0)

print("Z세대 (IsZen == 1)의 정규성 검정 결과:")
print(shapiro_gen1)

# 3. 등분산성 검정 (Levene's Test)
library(car) # Levene's Test를 위한 패키지
levene_result <- leveneTest(이직의도 ~ factor(IsZen), data = AA)

print("등분산성 검정 결과 (Levene's Test):")
print(levene_result)

################################################################################

table(filtered_data$W22Q18A)

filtered_data$의사소통 <- rowMeans(filtered_data[, c("W22Q24A", "W22Q24B", "W22Q24C")], na.rm = TRUE)
filtered_data$인재우대 <- rowMeans(filtered_data[, c("W22Q23A", "W22Q23B", "W22Q23C", "W22Q23D")], na.rm = TRUE)
   filtered_data$조직내신뢰 <- rowMeans(filtered_data[, c("W22Q24D", "W22Q24E", "W22Q24F")], na.rm = TRUE)
filtered_data$조직내혁신 <- rowMeans(filtered_data[, c("W22Q25A", "W22Q25B", "W22Q25C")], na.rm = TRUE)
filtered_data$능력중심 <- rowMeans(filtered_data[, c("W22Q25K", "W22Q25L")], na.rm = TRUE)
filtered_data$의사소통2 <- rowMeans(filtered_data[, c("W22Q22F", "W22Q22A", "W22Q22F")], na.rm = TRUE)

# Z세대 회귀분석
model_z <- lm(이직의도 ~ 의사소통2, data = filtered_data[filtered_data$IsZen == 1, ])
summary(model_z)

# 기성세대 회귀분석
model_gen0 <- lm(이직의도 ~ 의사소통2, data = filtered_data[filtered_data$IsZen == 0, ])
summary(model_gen0)

ggplot(filtered_data, aes(x = 의사소통2, y = 이직의도, color = factor(IsZen))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Z세대와 기성세대의 의사소통2 & 이직의도의 관계",
       x = "의사소통2", y = "이직의도", color = "세대")


# Z세대 회귀분석
model_z <- lm(이직의도 ~ 인재우대, data = filtered_data[filtered_data$IsZen == "Z세대", ])
summary(model_z)

# 기성세대 회귀분석
model_gen0 <- lm(이직의도 ~ 인재우대, data = filtered_data[filtered_data$IsZen == "기성세대", ])
summary(model_gen0)

ggplot(filtered_data, aes(x = 인재우대, y = 이직의도, color = factor(IsZen))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Z세대와 기성세대의 인재우대와 이직의도의 관계",
       x = "인재우대", y = "이직의도", color = "세대")





# 상호작용 효과를 포함한 회귀분석
interaction_model <- lm(이직의도 ~ 의사소통2 * IsZen, data = filtered_data)

# 결과 요약
summary(interaction_model)

################################################################################

ggplot(filtered_data[filtered_data$IsZen == 1, ], aes(x = factor(이직의도))) +
  geom_bar(fill = "red", alpha = 0.7) +
  labs(title = "Z세대의 이직의도 분포",
       x = "이직의도 (Likert 척도)", y = "빈도")

ggplot(filtered_data[filtered_data$IsZen == 0, ], aes(x = factor(이직의도))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "기성세대의 이직의도 분포",
       x = "이직의도 (Likert 척도)", y = "빈도")

# IsZen을 범주형 변수로 변환
filtered_data$IsZen <- factor(filtered_data$IsZen, levels = c(0, 1), labels = c("기성세대", "Z세대"))

# Levene's Test 실행
levene_result <- leveneTest(이직의도 ~ IsZen, data = filtered_data)

# 결과 출력
print("Levene's Test 결과:")
print(levene_result)

qqnorm(z_gen, main = "Z세대 QQ-Plot")
qqline(z_gen, col = "blue")

qqnorm(gen0, main = "기성세대 QQ-Plot")
qqline(gen0, col = "blue")

t.test(z_gen, gen0, var.equal = FALSE)

# Mann-Whitney U Test 수행
mann_whitney_result <- wilcox.test(이직의도 ~ IsZen, data = filtered_data)

# 결과 출력
print("Mann-Whitney U Test 결과:")
print(mann_whitney_result)


ggplot(filtered_data, aes(x = 인재우대, y = 이직의도)) +
  geom_point(aes(size = ..count.., alpha = ..count..), stat = "bin2d", bins = 10) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_alpha_continuous(range = c(0.2, 30)) +
  scale_size_continuous(range = c(1, 5)) +
  labs(
       x = "조직내 인재우대", y = "이직의도")




model_injae <- lm(이직의도 ~ 인재우대, data = filtered_data)

# 모델 요약 출력
summary(model_injae)

# Q-Q Plot
qqnorm(resid(model_injae))
qqline(resid(model_injae), col = "blue")

# Shapiro-Wilk Test
shapiro.test(resid(model_injae))

filtered_data$residuals <- resid(model_injae) # 모델 이름에 따라 변경
filtered_data$fitted_values <- model_injae$fitted.values # 모델 이름에 따라 변경

# Residuals vs. Fitted Plot
ggplot(filtered_data, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
       x = "적합값",
       y = "잔차")

ggplot(filtered_data, aes(x = 인재우대, y = 이직의도)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "인재우대와 이직의도의 관계 (회귀선 포함)",
       x = "인재우대", y = "이직의도")


# 회귀모델 생성
model_communication <- lm(이직의도 ~ 의사소통2, data = filtered_data)

# 모델 요약 출력
summary(model_communication)
confint(model_injae)
confint(model_communication)
filtered_data$residuals <- resid(model_communication) # 잔차
filtered_data$fitted_values <- model_communication$fitted.values # 적합값

# Residuals vs. Fitted Plot
ggplot(filtered_data, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "적합값", y = "잔차")


model_communication <- lm(이직의도 ~ 의사소통2, data = filtered_data) 

################################################################################

filtered_data$IsZen <- ifelse(filtered_data$IsZen == "Z세대", 1, 0)

# Z세대 회귀분석
z_model <- lm(이직의도 ~ 인재우대, data = filtered_data[filtered_data$IsZen == 1, ])
summary(z_model)

# 기성세대 회귀분석
gen0_model <- lm(이직의도 ~ 인재우대, data = filtered_data[filtered_data$IsZen == 0, ])
summary(gen0_model)

ggplot(filtered_data, aes(x = 인재우대, y = 이직의도, color = factor(IsZen, labels = c("기성세대", "Z세대")))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "세대별 인재우대와 이직의도 관계 비교",
       x = "인재우대", y = "이직의도", color = "세대")



# Z세대 회귀모델
z_model <- lm(이직의도 ~ 인재우대, data = filtered_data[filtered_data$IsZen == 1, ])
z_coef <- summary(z_model)$coefficients["인재우대", "Estimate"]
z_se <- summary(z_model)$coefficients["인재우대", "Std. Error"]

# 기성세대 회귀모델
gen0_model <- lm(이직의도 ~ 인재우대, data = filtered_data[filtered_data$IsZen == 0, ])
gen0_coef <- summary(gen0_model)$coefficients["인재우대", "Estimate"]
gen0_se <- summary(gen0_model)$coefficients["인재우대", "Std. Error"]

# 두 기울기 차이와 표준 오차
difference <- z_coef - gen0_coef
pooled_se <- sqrt(z_se^2 + gen0_se^2)

# z-statistic과 p-value
z_stat <- difference / pooled_se
p_value <- 2 * pnorm(-abs(z_stat))  # 양측검정

cat("기울기 차이:", difference, "\n")
cat("z-statistic:", z_stat, "\n")
cat("p-value:", p_value, "\n")


z_sample <- filtered_data %>% filter(IsZen == 1)
gen0_sample <- filtered_data %>% filter(IsZen == 0)

cat("Z세대 샘플 수:", nrow(z_sample), "\n")
cat("기성세대 샘플 수:", nrow(gen0_sample), "\n")

# 가중치 설정 (샘플 크기 비율의 역수)
z_weight <- nrow(gen0_sample) / nrow(z_sample)
gen0_weight <- 1

filtered_data <- filtered_data %>%
  mutate(weight = ifelse(IsZen == 1, z_weight, gen0_weight))

# Z세대 데이터 추출
z_data <- filtered_data[filtered_data$IsZen == 1, ]

# 기성세대 데이터 추출
gen0_data <- filtered_data[filtered_data$IsZen == 0, ]

# Z세대 회귀분석
z_model <- lm(이직의도 ~ 의사소통2, data = z_data)
summary(z_model)

# 기성세대 회귀분석
gen0_model <- lm(이직의도 ~ 의사소통2, data = gen0_data)
summary(gen0_model)

# 상호작용 효과를 포함한 회귀분석
interaction_model <- lm(이직의도 ~ 의사소통2 * IsZen, data = filtered_data)

# 결과 요약
summary(interaction_model)

ggplot(filtered_data, aes(x = 의사소통2, y = 이직의도, color = factor(IsZen, labels = c("기성세대", "Z세대")))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
       x = "의사소통", y = "이직의도", color = "세대")
