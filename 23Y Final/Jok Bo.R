library(tidyverse)
library(dplyr)
h<-read.delim("Satis.dat",header=TRUE)
h$area2<- c("서울","지방","해외")[match(h$area, c(1,2,3))]
h$area2<-factor(h$area2)
h$area2

summary(h$satis)
ggplot(h,aes(satis))+
  geom_histogram()

h$satisa<-scale(h$satis)
ggplot(h,aes(satisa))+
  geom_histogram()

summary(h$satis)
ggplot(h,aes(satis))+
  geom_histogram()
par(mfrow=c(1,1))
qqnorm(h$salary)
qqline(h$salary)
shapiro.test(h$salary)

cor.test(h[,2],h[,3])
round(cor(h[,-c(1,8,9)]),3)

h$area_co<-ifelse(h$area==2,1,0) ##지방
h$area_ab<-ifelse(h$area==3,1,0) ##해외
d<-lm(satis~motiv+envir+salary+stress+area_co+area_ab,data=h)
install.packages("car")
library(car)
resid<-residuals(d)
vif(d)
hist(resid)
durbinWatsonTest(resid)
shapiro.test(resid)
par(mfrow=c(2,2)) 
plot(d)


h$area_co<-ifelse(h$area==2,1,0) ##지방
h$area_ab<-ifelse(h$area==3,1,0) ##해외
e<-lm(salary~motiv+envir+satis+stress+area_co+area_ab,data=h)
library(car)
resid<-residuals(e)
vif(e)
hist(resid)
durbinWatsonTest(resid)
shapiro.test(resid)
par(mfrow=c(2,2)) 
plot(e)


