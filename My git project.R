AA<-100
##https://github.com/yoonnholo/Myuri

library(ggplot2)
library(tidyverse)
getwd()

spicenwolf<-c("Holo","Myuri","Law")
str(spicenwolf)
A<-c("Yoon","Han","Bin")
B<-c(6,9,15)
C<-c("F","F","M")
D<-data.frame(Name=A,Dick=B,Sex=C)
D
GodAnime=spicenwolf
View(hololo)
getwd()

Sorry=D
Sorry
A
AA
NewWorld<-"Hello"
NewWorld
install.packages("remotes")
remotes::install_github("anthonynorth/rscodeio")
rscodeio::install_theme()
D
factor(C)

install.packages("tidyverse")
library(tidyverse)
getwp()
getwd()

summary(hololo2)
summary(hololo2$ID)
table(hololo2$Species)
table(bts$btsposition)
table(bts$btsname,btsposition)

data("mpg")
View(mpg)
str(mpg)

A<-ggplot(data = mpg,aes(x=displ,y=hwy))
A+geom_point()
A+geom_histogram()

A <- ggplot(data = mpg, aes(x=year))
A+geom_histogram()
mpg
Z<-data.frame(mpg)
Z %>% 
  count(year)

B<-ggplot(data = mpg,aes(x=manufacturer,y=hwy))
B+geom_boxplot()

B<-ggplot(data=mpg, aes(x=displ,y=hwy))
B+stat_summary(fun= mean,geom = "bar")

install.packages("foreign")
library(foreign)
A
str(A)
read.spss("f11_h_youth.SAV", reencode = 'utf-8', to.data.frame = TRUE) -> A
A %>% 
  select(BYSID,GENDER,F11Y02001,F11Y05034,F11Y05048) ->BEFORE
B
names(BEFORE)<-c("ID","GENDER","EDU","INCOME","SAT")

b
names(b)<-c("ID","GENDER","EDU","INCOME","SAT")

str(BEFORE$EDU)
str(B$EDU)

levels(BEFORE$EDU)

BEFORE %>%
  filter(EDU!="-5")