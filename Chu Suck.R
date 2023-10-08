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

DATA135 %>% 
  select(month:arr_delay) -> DATA135
str(DATA135)
DATA135 %>% 
  mutate(delay=arr_delay-dep_delay) -> DATA135
DATA135 %>% 
  mutate(GorB=ifelse(delay>=0,"Good","Bad")) -> DATA135

table(DATA135$GorB)
DATA135 %>% 
  count(GorB)


DATA135$air_time <- with(DATA135,(
dep_hour <- floor(dep_time / 100) %>% 
dep_minute <- dep_time %% 100 %>% 
arr_hour <- floor(arr_time / 100) %>% 
arr_minute <- arr_time %% 100 %>% 
hour_diff <- arr_hour - dep_hour %>% 
minute_diff <- arr_minute - dep_minute %>% 
if (minute_diff < 0) {
  hour_diff <- hour_diff - 1
  minute_diff <- 60 + minute_diff
} %>% 
air_time <- hour_diff * 60 + minute_diff))

DATA135$air_time <- with(DATA135, {
  dep_hour <- floor(dep_time / 100)
  dep_minute <- dep_time %% 100
  arr_hour <- floor(arr_time / 100)
  arr_minute <- arr_time %% 100
  hour_diff <- arr_hour - dep_hour
  minute_diff <- arr_minute - dep_minute
  ifelse (minute_diff < 0,
    hour_diff <- hour_diff - 1,
    minute_diff <- minute_diff + 60)  # 음수 분을 양수로 변경
  
  air_time <- hour_diff * 60 + minute_diff
  return(air_time)  # 결과 반환
})
  

DATA135 %>% 
  filter(!is.na(GorB)) %>% 
  group_by(GorB) %>% 
  summarise(MEAN=mean(air_time),
            MAX=max(delay),
            MIN=min(delay),
            n=n()) -> RE
RE
FRAME
firstname<-c("J","L","L","P","Y")
FRAME2<-data.frame(firstname)
bind_cols(FRAME,FRAME2) -> FRAME3
str(FRAME3)

FRAME3$firstname<-as.factor(FRAME3$firstname)

name<-c("Yoon","Tae","Jun")
position<-c("sup","adc","mid")
tier<-c("Bron","Plat","Plat")
data1<-data.frame(name,position,tier)

nation<-c("Kor","Can","Can")
data2<-data.frame(name,tier,nation)

data1 %>% 
  right_join(data2,by="name",suffix=c("_1","_2"))

data1 %>% 
  inner_join(data2, by="name")

data1 %>% 
  full_join(data2,by="name")

memberframe<-data.frame(name="Hun",tier="Sil",nation="Kor")
memberframe2<-data.frame(name="Jae",position="Jg",tier="Gol")
bind_rows(data2,memberframe) -> data2
bind_rows(data1,memberframe2) ->data1


