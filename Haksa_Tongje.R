### 학사논문 통제변수 정리하기 ###
table(new_data$Gender) ## 남성 : 1 | 여성 : 2
table(new_data$Married) ## 미혼 : 1 | 기혼 : 2 | 이혼, 사별 : 3
table(new_data$Edu) ## 고졸이하 : 1 | 전문대졸 : 2 | 대졸 : 3 | 대학원졸 : 4
new_data$Edu <- with(new_data, ifelse(Edu %in% c(1, 2, 3), 1, 
                                      ifelse(Edu == 4, 2, 
                                             ifelse(Edu == 5, 3, 
                                                    ifelse(Edu %in% c(6, 7), 4, Edu)))))
table(new_data$Major)
new_data$Major <- NULL


########### 기술통계량 #############
selected_vars <- new_data[, c("Satisfaction", "Commitment", "Turnover", # 종속 변수
                              "Hierarchical",                         # 독립 변수
                              "IsZGen",                               # 조절 변수
                              "LowCommunicate",                       # 매개 변수
                              "Gender", "Married", "Edu")]            # 통제 변수

library(psych)
describe(selected_vars)


nominal_vars <- new_data[, c("Gender", "Married", "Edu", "IsZGen")]

# 각 변수별로 표본 수와 비율 계산
nominal_summary <- lapply(nominal_vars, function(x) {
  freq <- table(x)
  proportion <- prop.table(freq) * 100
  data.frame(Frequency = freq, Proportion = round(proportion, 2))
})

# 결과 출력
nominal_summary



research_vars <- new_data[, c("Satisfaction", "Commitment", "Turnover", # 종속 변수
                              "Hierarchical",                         # 독립 변수
                              "IsZGen",                               # 조절 변수
                              "LowCommunicate"                      # 매개 변수
                              )]            # 통제 변수

# 상관계수 계산
cor_matrix <- cor(research_vars, use = "complete.obs")

# 상관계수 행렬 출력
cor_matrix


new_data$Gender <- factor(new_data$Gender)
new_data$Married <- factor(new_data$Married)
new_data$Edu <- factor(new_data$Edu)

satisfaction_model <- lm(Satisfaction ~ Hierarchical + LowCommunicate + Gender + Married + Edu, data = new_data)
summary(satisfaction_model)


satisfaction_model <- lm(Satisfaction ~ Hierarchical + Gender + Married + Edu, data = new_data)
summary(satisfaction_model)




## 조절변수를 MZ에서 업무분야로 변경
table(RAW$W22Q04)
table(RAW$W22IND1)
