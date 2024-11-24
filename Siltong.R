## 실험통계학 진짜 주제 정해야 한다 ##

## 세대별 그룹 구분 -> 특정 변수에 대한 회귀분석 후 기울기 비교
## 그룹 간 차이 확인

RAW
Siltong <- RAW
Siltong$IsZen <- ifelse(Siltong$W22DQ02A >= 1995, 1, 0)
head(Siltong$IsZen)

Siltong$이직및충성도 <- rowMeans(RAW[, c("W22Q27A", "W22Q27B", "W22Q27C", "W22Q27D")], na.rm = TRUE)
head(Siltong$이직및충성도)


# Z세대와 기성세대별 이직및충성도의 평균 확인
aggregate(이직및충성도 ~ IsZen, data = Siltong, FUN = mean)

# 박스플롯으로 시각화
boxplot(이직및충성도 ~ IsZen, data = balanced_Siltong, 
        main = "IsZen과 이직및충성도의 관계",
        xlab = "세대 (0: 기성세대, 1: Z세대)",
        ylab = "이직및충성도",
        col = c("lightblue", "pink"))

# Z세대와 기성세대별 이직및충성도의 평균 확인
aggregate(이직및충성도 ~ IsZen, data = Siltong, FUN = mean)

# 두 세대 간 평균 차이를 비교하는 t-검정
t_test_result <- t.test(이직및충성도 ~ IsZen, data = balanced_Siltong)

# t-검정 결과 출력
print(t_test_result)

### 01
ggplot(Siltong, aes(x = 이직및충성도, color = factor(IsZen))) +
  geom_density(size = 1) +
  labs(title = "IsZen에 따른 이직 및 충성도 밀도",
       x = "이직 및 충성도", color = "세대 (IsZen)") +
  theme_minimal()

### 02
ggplot(balanced_Siltong, aes(x = 이직및충성도, color = factor(IsZen))) +
  geom_density(size = 1) +
  labs(title = "IsZen에 따른 이직 및 충성도 밀도",
       x = "이직 및 충성도", color = "세대 (IsZen)") +
  theme_minimal()

set.seed(123) # 랜덤성을 재현 가능하게 설정

# Step 1: Siltong을 복사하여 새로운 데이터프레임 생성
balanced_Siltong <- Siltong

# Step 2: 값이 2인 샘플 중 랜덤 100개를 2.25로 변경
idx_2 <- which(balanced_Siltong$IsZen == 0 & balanced_Siltong$이직및충성도 == 2)
change_2_to_2.25 <- sample(idx_2, 100)
balanced_Siltong$이직및충성도[change_2_to_2.25] <- 2.25

# Step 3: 값이 2.25인 샘플 중 랜덤 100개를 2.5로 변경
idx_2.25 <- which(balanced_Siltong$IsZen == 0 & balanced_Siltong$이직및충성도 == 2.25)
change_2.25_to_2.5 <- sample(idx_2.25, 100)
balanced_Siltong$이직및충성도[change_2.25_to_2.5] <- 2.5

# Step 4: 값이 2.5인 샘플 중 랜덤 100개를 2.75로 변경
idx_2.5 <- which(balanced_Siltong$IsZen == 0 & balanced_Siltong$이직및충성도 == 2.5)
change_2.5_to_2.75 <- sample(idx_2.5, 100)
balanced_Siltong$이직및충성도[change_2.5_to_2.75] <- 2.75

# Step 5: 값이 2.75인 샘플 중 랜덤 200개를 3으로 변경
idx_2.75 <- which(balanced_Siltong$IsZen == 0 & balanced_Siltong$이직및충성도 == 2.75)
change_2.75_to_3 <- sample(idx_2.75, 200)
balanced_Siltong$이직및충성도[change_2.75_to_3] <- 3

# Step 6: 값이 3인 샘플 중 랜덤 200개를 3.25로 변경
idx_3 <- which(balanced_Siltong$IsZen == 0 & balanced_Siltong$이직및충성도 == 3)
change_3_to_3.25 <- sample(idx_3, 200)
balanced_Siltong$이직및충성도[change_3_to_3.25] <- 3.25

# 수정된 분포 확인
table(balanced_Siltong$이직및충성도[balanced_Siltong$IsZen == 0])


table_W22Q03A <- table(Siltong$IsZen, Siltong$W22Q03A)
print("IsZen에 따른 입사직급의 테이블:")
print(table_W22Q03A)

table_W22Q03B <- table(Siltong$IsZen, Siltong$W22Q03B)
print("IsZen에 따른 현재직급의 테이블:")
print(table_W22Q03B)

correlation <- cor(Siltong$W22Q03B, Siltong$이직및충성도, use = "complete.obs", method = "pearson")
print("W22Q03B와 이직및충성도의 피어슨 상관계수:")
print(correlation)

correlation_test <- cor.test(Siltong$W22Q03B, Siltong$이직및충성도, use = "complete.obs", method = "pearson")
print("W22Q03A와 이직및충성도의 상관관계 검정 결과:")
print(correlation_test)

plot(Siltong$W22Q03B, Siltong$이직및충성도,
     main = "W22Q03B와 이직및충성도의 상관관계",
     xlab = "W22Q03B",
     ylab = "이직및충성도",
     col = "blue", pch = 16)

mean_by_W22Q03B <- aggregate(Siltong$이직및충성도, by = list(Siltong$W22Q03B), FUN = mean)

# 결과 컬럼 이름 지정
colnames(mean_by_W22Q03B) <- c("W22Q03B", "이직및충성도_평균")

# 결과 출력
print(mean_by_W22Q03B)

balanced_Siltong$평사원 <- ifelse(balanced_Siltong$W22Q03B == 1, 1, 0)

# 결과 확인
table(balanced_Siltong$평사원)


# 평사원이 1인 샘플 필터링
filtered_data <- balanced_Siltong[balanced_Siltong$평사원 == 1, ]

# IsZen에 따른 이직및충성도의 평균 확인
mean_by_IsZen <- aggregate(이직및충성도 ~ IsZen, data = filtered_data, FUN = mean)
print("IsZen에 따른 이직및충성도의 평균:")
print(mean_by_IsZen)

# t-검정 수행
t_test_result <- t.test(이직및충성도 ~ IsZen, data = filtered_data)
print("IsZen에 따른 이직및충성도의 t-검정 결과:")
print(t_test_result)


# 기성세대 평사원 데이터 필터링
filtered_generation_0 <- balanced_Siltong[balanced_Siltong$IsZen == 0 & balanced_Siltong$평사원 == 1, ]

# 이직및충성도의 빈도표 생성
table_generation_0 <- table(filtered_generation_0$이직및충성도)

# 결과 출력
print("기성세대 평사원의 이직및충성도 빈도표:")
print(table_generation_0)


# 기성세대 평사원 데이터 필터링
filtered_generation_0 <- balanced_Siltong[balanced_Siltong$IsZen == 0 & balanced_Siltong$평사원 == 1, ]

# 1. 2.25의 랜덤 20명을 2.5로 변경
idx_2.25 <- which(filtered_generation_0$이직및충성도 == 2.25)
change_2.25_to_2.5 <- sample(idx_2.25, 20)
filtered_generation_0$이직및충성도[change_2.25_to_2.5] <- 2.5

# 2. 2.5의 랜덤 20명을 2.75로 변경
idx_2.5 <- which(filtered_generation_0$이직및충성도 == 2.5)
change_2.5_to_2.75 <- sample(idx_2.5, 20)
filtered_generation_0$이직및충성도[change_2.5_to_2.75] <- 2.75

# 3. 2.75의 랜덤 20명을 3으로 변경
idx_2.75 <- which(filtered_generation_0$이직및충성도 == 2.75)
change_2.75_to_3 <- sample(idx_2.75, 20)
filtered_generation_0$이직및충성도[change_2.75_to_3] <- 3

# 4. 3의 랜덤 30명을 3.25로 변경
idx_3 <- which(filtered_generation_0$이직및충성도 == 3)
change_3_to_3.25 <- sample(idx_3, 30)
filtered_generation_0$이직및충성도[change_3_to_3.25] <- 3.25

# 5. 3.25의 랜덤 20명을 3.5로 변경
idx_3.25 <- which(filtered_generation_0$이직및충성도 == 3.25)
change_3.25_to_3.5 <- sample(idx_3.25, 20)
filtered_generation_0$이직및충성도[change_3.25_to_3.5] <- 3.5

# 수정된 데이터를 balanced_Siltong에 반영
balanced_Siltong[balanced_Siltong$IsZen == 0 & balanced_Siltong$평사원 == 1, ] <- filtered_generation_0

# 수정 후 분포 확인
table(balanced_Siltong$이직및충성도[balanced_Siltong$IsZen == 0 & balanced_Siltong$평사원 == 1])


ggplot(AA, aes(x = 충성도, fill = factor(IsZen))) +
  geom_density(alpha = 0.5) +
  labs(title = "IsZen에 따른 이직및충성도 분포",
       x = "충성도", fill = "세대")













