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

## 통제변수 삽입
new_data$Gender <- RAW$W22DQ01
new_data$Married <- RAW$W22DQ03
new_data$Edu <- RAW$W22DQ04
new_data$Major <- RAW$W22DQ041

## 결측치 확인
colSums(is.na(new_data))

## 이상치 확인
check_outliers <- function(x) {
  sum(x < 1 | x > 5, na.rm = TRUE)
}
sapply(new_data, check_outliers)
lapply(new_data, table, useNA = "ifany")

## 변수 합치기
new_data$Hierarchical <- rowSums(new_data[, c("Hierarchical1", "Hierarchical2", "Hierarchical3")], na.rm = TRUE) / 3
new_data$LowCommunicate <- rowSums(new_data[, c("LowCommunicate1", "LowCommunicate2", "LowCommunicate3", 
                                                "LowCommunicate4", "LowCommunicate5")], na.rm = TRUE) / 5
new_data$Satisfaction <- rowSums(new_data[, c("Satisfaction1", "Satisfaction2", "Satisfaction3", "Satisfaction4")], na.rm = TRUE) / 4
new_data$Commitment <- rowSums(new_data[, c("Commitment1", "Commitment2")], na.rm = TRUE) / 2
new_data$Turnover <- rowSums(new_data[, c("Turnover1", "Turnover2")], na.rm = TRUE) / 2

table(new_data$Hierarchical)
table(new_data$Satisfaction)


## MZ 분류하기

hist(new_data$MZ)
table(new_data$MZ)
new_data$IsZGen <- ifelse(new_data$MZ >= 1995 & new_data$MZ <= 2004, 1, 0)

##########################################################################

# 직무 만족에 대한 회귀 분석
satisfaction_model <- lm(Satisfaction ~ Hierarchical, data = new_data)
summary(satisfaction_model)

# 조직 몰입에 대한 회귀 분석
commitment_model <- lm(Commitment ~ Hierarchical, data = new_data)
summary(commitment_model)

# 이직 의도에 대한 회귀 분석
turnover_model <- lm(Turnover ~ Hierarchical, data = new_data)
summary(turnover_model)

##########################################################################

# 매개효과 분석

# 1단계: 독립변수 -> 종속변수
satisfaction_model <- lm(Satisfaction ~ Hierarchical, data = new_data)
commitment_model <- lm(Commitment ~ Hierarchical, data = new_data)
turnover_model <- lm(Turnover ~ Hierarchical, data = new_data)

# 2단계: 독립변수 -> 매개변수
mediator_model <- lm(LowCommunicate ~ Hierarchical, data = new_data)

# 3단계: 매개변수 -> 종속변수
satisfaction_mediator <- lm(Satisfaction ~ LowCommunicate, data = new_data)
commitment_mediator <- lm(Commitment ~ LowCommunicate, data = new_data)
turnover_mediator <- lm(Turnover ~ LowCommunicate, data = new_data)

# 4단계: 독립변수와 매개변수를 모두 포함
satisfaction_full <- lm(Satisfaction ~ Hierarchical + LowCommunicate, data = new_data)
commitment_full <- lm(Commitment ~ Hierarchical + LowCommunicate, data = new_data)
turnover_full <- lm(Turnover ~ Hierarchical + LowCommunicate, data = new_data)

# 결과 확인
summary(satisfaction_model)
summary(commitment_model)
summary(turnover_model)
summary(mediator_model)
summary(satisfaction_mediator)
summary(commitment_mediator)
summary(turnover_mediator)
summary(satisfaction_full)
summary(commitment_full)
summary(turnover_full)

##########################################################################

# Z세대 데이터 필터링
z_gen_data <- subset(new_data, IsZGen == 1)

# 1단계: 독립변수 -> 종속변수
satisfaction_model <- lm(Satisfaction ~ Hierarchical, data = z_gen_data)
commitment_model <- lm(Commitment ~ Hierarchical, data = z_gen_data)
turnover_model <- lm(Turnover ~ Hierarchical, data = z_gen_data)

# 2단계: 독립변수 -> 매개변수
mediator_model <- lm(LowCommunicate ~ Hierarchical, data = z_gen_data)

# 3단계: 매개변수 -> 종속변수
satisfaction_mediator <- lm(Satisfaction ~ LowCommunicate, data = z_gen_data)
commitment_mediator <- lm(Commitment ~ LowCommunicate, data = z_gen_data)
turnover_mediator <- lm(Turnover ~ LowCommunicate, data = z_gen_data)

# 4단계: 독립변수와 매개변수를 모두 포함
satisfaction_full <- lm(Satisfaction ~ Hierarchical + LowCommunicate, data = z_gen_data)
commitment_full <- lm(Commitment ~ Hierarchical + LowCommunicate, data = z_gen_data)
turnover_full <- lm(Turnover ~ Hierarchical + LowCommunicate, data = z_gen_data)

# 결과 확인
summary(satisfaction_model)
summary(commitment_model)
summary(turnover_model)
summary(mediator_model)
summary(satisfaction_mediator)
summary(commitment_mediator)
summary(turnover_mediator)
summary(satisfaction_full)
summary(commitment_full)
summary(turnover_full)



# 직무 만족에 대한 조절효과 확인
satisfaction_mz_model <- lm(Satisfaction ~ Hierarchical * IsZGen, data = new_data)
summary(satisfaction_mz_model)

# 조직 몰입에 대한 조절효과 확인
commitment_mz_model <- lm(Commitment ~ Hierarchical * IsZGen, data = new_data)
summary(commitment_mz_model)

# 이직 의도에 대한 조절효과 확인
turnover_mz_model <- lm(Turnover ~ Hierarchical * IsZGen, data = new_data)
summary(turnover_mz_model)


# 매개변수 LowCommunicate와 조절변수 IsZGen의 상호작용 포함
satisfaction_mediation_mod <- lm(Satisfaction ~ Hierarchical * IsZGen + LowCommunicate * IsZGen, data = new_data)
commitment_mediation_mod <- lm(Commitment ~ Hierarchical * IsZGen + LowCommunicate * IsZGen, data = new_data)
turnover_mediation_mod <- lm(Turnover ~ Hierarchical * IsZGen + LowCommunicate * IsZGen, data = new_data)

# 결과 확인
summary(satisfaction_mediation_mod)
summary(commitment_mediation_mod)
summary(turnover_mediation_mod)




