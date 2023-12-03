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
