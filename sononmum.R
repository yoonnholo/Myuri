##소논문 써보자 씨발

data <- read.csv("KCYPS2018m1Yw4.csv")
data

data %>% 
  select(HID,PID,YGENDERw4,YDLQ2A01w4,YMDA1C01w4:YMDA1C15w4,YDLQ1A01w4:YDLQ1A15w4,YPSY4B01w4:YPSY4B06w4) %>% 
  rowwise() %>%
  mutate(SmartPhone = sum(c_across(starts_with("YMDA1C01w4"):starts_with("YMDA1C15w4")), na.rm = TRUE)) %>% 
  mutate(Real = sum(c_across(starts_with("YDLQ1A01w4"):starts_with("YDLQ1A15w4")), na.rm = TRUE)) %>% 
  mutate(Agressive = sum(c_across(starts_with("YPSY4B01w4"):starts_with("YPSY4B06w4")), na.rm = TRUE)) %>% 
  rename(Cyber_Fly = YDLQ2A01w4) %>% 
  rename(Gender = YGENDERw4) %>% 
  select(-starts_with("YMDA1C01w4"): -starts_with("YMDA1C15w4")) %>% 
  select(-starts_with("YDLQ1A01w4"): -starts_with("YDLQ1A15w4")) %>% 
  select(-starts_with("YPSY4B01w4"): -starts_with("YPSY4B06w4")) %>% 
  na.omit() -> new

table(new$Cyber_Fly)
table(new$Real)
table(new$SmartPhone)
table(new$Agressive)
