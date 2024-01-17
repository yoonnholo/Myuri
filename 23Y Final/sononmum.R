##소논문 써보자 아아악

data <- read.csv("KCYPS2018m1Yw4.csv")
data

data %>% 
  select(HID,PID,YGENDERw4,YDLQ2A01w4,YMDA1C01w4:YMDA1C15w4,YDLQ1A01w4:YDLQ1A15w4,YPSY4B01w4:YPSY4B06w4) %>% 
  rowwise() %>%
  mutate(SmartPhone = sum(c_across(starts_with("YMDA1C01w4"):starts_with("YMDA1C15w4")), na.rm = TRUE)) %>% 
  mutate(Real_Misdeed = sum(c_across(starts_with("YDLQ1A01w4"):starts_with("YDLQ1A15w4")), na.rm = TRUE)) %>% 
  mutate(Agression = sum(c_across(starts_with("YPSY4B01w4"):starts_with("YPSY4B06w4")), na.rm = TRUE)) %>% 
  rename(Cyber_Misdeed = YDLQ2A01w4) %>% 
  rename(Gender = YGENDERw4) %>% 
  rename(HomeID = HID) %>% 
  rename(PersonalID = PID) %>% 
  select(-starts_with("YMDA1C01w4"): -starts_with("YMDA1C15w4")) %>% 
  select(-starts_with("YDLQ1A01w4"): -starts_with("YDLQ1A15w4")) %>% 
  select(-starts_with("YPSY4B01w4"): -starts_with("YPSY4B06w4")) %>% 
  na.omit() -> new

view(new)

## 종속변인
table(new$Cyber_Misdeed)
## 독립변인
table(new$SmartPhone)
## 통제변인
table(new$Real_Misdeed)
table(new$Agression)


##-------------------------------------------------##

data <- read.csv("KCYPS2018m1Yw4.csv")
data %>% 
  select(HID,PID,YGENDERw4,YDLQ2A01w4,YMDA1C01w4:YMDA1C15w4,YDLQ1A15w4,YPSY4B01w4:YPSY4B06w4) -> select_data

data %>% 
  select(HID,PID,YGENDERw4,YDLQ2A01w4,YTIM1L01w4, YTIM1L02w4,YDLQ1A15w4,YPSY4B01w4:YPSY4B06w4) -> new

## 역산 필요 변수:   5번, 10번, 15번 = YMDA1C05w4, YMDA1C10w4, YMDA1C15w4

str(select_data)

select_data %>% 
  mutate(YMDA1C05w4 = case_when(
    YMDA1C05w4 == 1 ~ 4,
    YMDA1C05w4 == 2 ~ 3,
    YMDA1C05w4 == 3 ~ 2,
    YMDA1C05w4 == 4 ~ 1)) %>% 
  mutate(YMDA1C10w4 = case_when(
    YMDA1C10w4 == 1 ~ 4,
    YMDA1C10w4 == 2 ~ 3,
    YMDA1C10w4 == 3 ~ 2,
    YMDA1C10w4 == 4 ~ 1)) %>% 
  mutate(YMDA1C15w4 = case_when(
    YMDA1C15w4 == 1 ~ 4,
    YMDA1C15w4 == 2 ~ 3,
    YMDA1C15w4 == 3 ~ 2,
    YMDA1C15w4 == 4 ~ 1)) -> true_data

  true_data %>% 
    rename(HomeID = HID) %>% 
    rename(PersonalID = PID) %>% 
    rename(Gender = YGENDERw4) %>% 
    rename(Real_Misdeed = YDLQ1A15w4) %>% 
    rename(Cyber_Misdeed = YDLQ2A01w4) -> true_data
  
  true_data %>% 
    rowwise() %>%
    mutate(Smartphone = sum(c_across(starts_with("YMDA1C01w4"):starts_with("YMDA1C15w4")), na.rm = TRUE)) %>% 
    mutate(Agression = sum(c_across(starts_with("YPSY4B01w4"):starts_with("YPSY4B06w4")), na.rm = TRUE)) %>% 
    select(-starts_with("YMDA1C01w4"): -starts_with("YMDA1C15w4")) %>% 
    select(-starts_with("YPSY4B01w4"): -starts_with("YPSY4B06w4")) %>% 
    na.omit() %>% 
    filter(Smartphone != 0) %>% 
    mutate(Gender = case_when(
      Gender == 1 ~ 0,
      Gender == 2 ~ 1)) -> sum_data
  

  
table(sum_data$Cyber_Misdeed)
table(sum_data$Smartphone)
table(sum_data$Gender)
table(sum_data$Real_Misdeed)
table(sum_data$Agression)


dependent_var <- "Cyber_Misdeed"
independent_var <- "Smartphone"
control_vars <- c("Gender", "Real_Misdeed", "Agression")

model <- lm(paste(dependent_var, "~", independent_var, "+", paste(control_vars, collapse = "+")), data = sum_data)

summary(model)


##-----------------------------------

new %>% 
  rename(HomeID = HID) %>% 
  rename(PersonalID = PID) %>% 
  rename(Gender = YGENDERw4) %>% 
  rename(Real_Misdeed = YDLQ1A15w4) %>% 
  rename(Cyber_Misdeed = YDLQ2A01w4) %>% 
  mutate(Gender = case_when(
    Gender == 1 ~ 0,
    Gender == 2 ~ 1)) -> new

new %>% 
  na.omit() -> new

new %>% 
  rowwise() %>%
  mutate(Agression = sum(c_across(starts_with("YPSY4B01w4"):starts_with("YPSY4B06w4")), na.rm = TRUE)) %>%
  mutate(Computer_time = sum(c_across(starts_with("YTIM1L01w4"):starts_with("YTIM1L02w4")), na.rm = TRUE)) %>% 
  select(-starts_with("YTIM1L01w4"): -starts_with("YTIM1L02w4")) %>% 
  select(-starts_with("YPSY4B01w4"): -starts_with("YPSY4B06w4")) -> new
  
new %>% 
filter(Cyber_Misdeed != 1) -> neww
  


table(new$Cyber_Misdeed)
table(new$Computer_time)
table(new$Gender)
table(new$Agression)
table(new$Real_Misdeed)

dependent_var <- "Cyber_Misdeed"
independent_var <- "Computer_time"
control_vars <- c("Real_Misdeed", "Gender","Agression")

model <- lm(paste(dependent_var, "~", independent_var, "+", paste(control_vars, collapse = "+")), data = new)

summary(model)

## 모형 확정, 기본적 기술통계, 회귀식과 회귀표

table(new$Real_Misdeed)

summary(new$Cyber_Misdeed)
var(new$Cyber_Misdeed)
sd(new$Cyber_Misdeed)

summary(new$Agression)
var(new$Agression)
sd(new$Agression)

new %>% 
  select(Gender:Computer_time) -> newnew


##---------------------------------

cor_matrix <- cor(newnew)
str(cor_matrix)



