##소논문 써보자 씨발

data <- read.csv("KCYPS2018m1Yw4.csv")
data

YDLQ1A01
YDLQ1A02
YDLQ1A03
YDLQ1A04
YDLQ1A05
YDLQ1A06
YDLQ1A07
YDLQ1A08
YDLQ1A09
YDLQ1A10
YDLQ1A11
YDLQ1A12
YDLQ1A13
YDLQ1A14
YDLQ1A15

str(data)

fly_data <- data %>% 
  select(YDLQ1A01w4:YDLQ1A15w4)
summary(fly_data)

fly_data2 <- fly_data %>%
  filter(!(YDLQ1A01w4 == 1 & YDLQ1A02w4 == 1 & YDLQ1A03w4 == 1 & YDLQ1A04w4 == 1 & YDLQ1A05w4 == 1 &
             YDLQ1A06w4 == 1 & YDLQ1A07w4 == 1 & YDLQ1A08w4 == 1 & YDLQ1A09w4 == 1 & YDLQ1A10w4 == 1 &
             YDLQ1A11w4 == 1 & YDLQ1A12w4 == 1 & YDLQ1A13w4 == 1 & YDLQ1A14w4 == 1 & YDLQ1A15w4 == 1))

summary(fly_data2)

smoking <- fly_data2 %>% 
  select(YDLQ1A01w4)
table(smoking)

data %>% 
  select(YDLQ2A01w4:YDLQ2A15w4) -> cyber
summary(cyber)

cyber %>% 
  select(YDLQ2A03w4) %>% 
  table()
