## 추석 기념 R 돌아보기

Name <- c("Yoon", "Jun", "Dong", "Nam", "Bin")
Number <- c(19, 18, 21, 21, 19)
Type <- c("Ori", "Ori", "Ip", "Ip", "Ori")
ID <- c(1,2,3,4,5)

FRAME <- data.frame(ID,Name,Number,Type)
FRAME
str(FRAME)
FRAME$Type <- factor(FRAME$Type, levels<-c("Ori","Ip"))
FRAME$Age <- c(25,26,29,29,27)
mean(FRAME$Age,na.rm = TRUE)
FRAME[1,5]<-25
summary(FRAME$Age)
table(FRAME$Age)

library(nycflights13)
head(flights)
DATA <- data.frame(flights)
DATA
str(DATA)

DATA %>% 
  filter(month %in% c(1,3,5)) -> DATA135
table(DATA135$month)
table(DATA$month)
