spicenwolf<-c("Holo","Law","Myuri")
spicenwolf
GodAnime=spicenwolf
GodAnime
spice<-factor(spicenwolf)
spice
structure(spice)
str(spice)
A<-c("Holo","Myuri","Law")
B<-c('F','F','M')
C<-c(1,2,3)
factor(A)
factor(B)
D
D<-100
D
D<-data.frame(A="name",B="sex",C="ID")
D
D<-data.frame(A=name,B=sex,C=ID)
D<-data.frame(name=A, sex=B, ID=C)
D
str(spicenwolf)
structure(spicenwolf)
factor(spicenwolf)
str(B)
factor(B)
str(D)
D
getwd()
setwd()

install.packages("readxl")
library(readxl)
S<-read_excel("hololo.xlsx")
S
S<-read_excel("hololo.xlsx")
getwd()
read_excel("hololo")
read_excel("hololo.xlsx")
AA<-read_excel("hololo.xlsx")
AA<-read_excel("hololo.xlsx")
hololo
read_xlsx("hololo.xlsx")
View(hololo)
structure(hololo)
str(hololo)

summary(hololo)
summary(hololo$ID)
summary(D$sex)
getwd()
library(readxl)
Z<-read_xlsx(hololo.xlsx)
hololo
hololo.xlsx
"hololo.xlsx"
Z<-read_excel("hololo.xlsx")
read_excel("Home/Desktop/R save/hololo.xlsx")

getwd()

read_excel("/Users/Yoonhyuk_Jeong/Desktop/Rsave/hololo.xlsx")
AA <- read_excel("/Users/Yoonhyuk_Jeong/Desktop/Rsave/hololo.xlsx")
AA
read_excel("Desktop/Rsave/hololo.xlsx")
read_excel("hololo.xlsx")

install.packages("remotes")
remotes::install_github("anthonynorth/rscodeio")

library(readxl)
read_excel("Desktop/RSave/hololo.xlsx")
AA
View(hololo)
str(hololo)
ZZ<-617
D

New<-c(5,10,99,8)
New
New %>%
  abs() %>%
  mean() %>%
  sqrt()

install.packages("nycflights13", repos = "http://cran.us.r-project.org")
library(nycflights13)
head(flights)
show(flights)
flight_df<-data.frame(flights)
str(flight_df)
flight_df %>%
  filter(month==2)

flight_df %>%
  filter(month==2, day==1)

flight_df %>%
  select(year:day)

date_df <- data.frame(flight_df %>%
                        select(year:day)
)
date_df
date_df
