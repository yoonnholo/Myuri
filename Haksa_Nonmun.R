library(haven)
RAW <- read_sav("RAW.sav")
View(RAW)

table(RAW$W22DQ02A)
class(RAW)

new_data <- data.frame(
  MZ = RAW$W22DQ02A,
  Hierarchical1 = RAW$W22Q25G,
  Hierarchical2 = RAW$W22Q25H,
  Hierarchical3 = RAW$W22Q25I,
  LowCommunicate1 = RAW$W22Q22A,
  LowCommunicate2 = RAW$W22Q22F,
  LowCommunicate3 = RAW$W22Q24A,
  LowCommunicate4 = RAW$W22Q24B,
  LowCommunicate5 = RAW$W22Q24C,
  Satisfaction1 = RAW$W22Q26A,
  Satisfaction2 = RAW$W22Q26B,
  Satisfaction3 = RAW$W22Q26C,
  Satisfaction4 = RAW$W22Q261,
  Commitment1 = RAW$W22Q27B,
  Commitment2 = RAW$W22Q27D,
  Turnover1 = RAW$W22Q27A,
  Turnover2 = RAW$W22Q27C
)

## LowCommunicate 5문항 전부, Turnover2 -> 역처리
new_data$LowCommunicate1 <- 6 - new_data$LowCommunicate1
new_data$LowCommunicate2 <- 6 - new_data$LowCommunicate2
new_data$LowCommunicate3 <- 6 - new_data$LowCommunicate3
new_data$LowCommunicate4 <- 6 - new_data$LowCommunicate4
new_data$LowCommunicate5 <- 6 - new_data$LowCommunicate5
new_data$Turnover2 <- 6 - new_data$Turnover2

## 결측치 확인
colSums(is.na(new_data))

## 이상치 확인
check_outliers <- function(x) {
  sum(x < 1 | x > 5, na.rm = TRUE)
}
sapply(new_data, check_outliers)
lapply(new_data, table, useNA = "ifany")

## 변수 합치기
new_data$Hierarchical <- rowSums(new_data[, c("Hierarchical1", "Hierarchical2", "Hierarchical3")], na.rm = TRUE)
new_data$LowCommunicate <- rowSums(new_data[, c("LowCommunicate1", "LowCommunicate2", "LowCommunicate3", 
                                                "LowCommunicate4", "LowCommunicate5")], na.rm = TRUE)
new_data$Satisfaction <- rowSums(new_data[, c("Satisfaction1", "Satisfaction2", "Satisfaction3", "Satisfaction4")], na.rm = TRUE)
new_data$Commitment <- rowSums(new_data[, c("Commitment1", "Commitment2")], na.rm = TRUE)
new_data$Turnover <- rowSums(new_data[, c("Turnover1", "Turnover2")], na.rm = TRUE)

## MZ 분류하기

hist(new_data$MZ)
table(new_data$MZ)



set.seed(123)  # 재현 가능성을 위해 시드 설정

# 비Z그룹 데이터 필터링
non_z_data <- new_data[new_data$IsZGen == 0, ]

# 573개의 표본을 랜덤으로 추출하고 분포 검토
sample_non_z <- non_z_data[sample(nrow(non_z_data), 573), ]

# 대표성 확인: 주요 변수 평균 비교
colMeans(sample_non_z[, c("Hierarchical", "LowCommunicate", "Satisfaction", "Commitment", "Turnover")])
colMeans(non_z_data[, c("Hierarchical", "LowCommunicate", "Satisfaction", "Commitment", "Turnover")])



