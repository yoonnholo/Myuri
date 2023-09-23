a %>% 
  select(BYSID,GENDER,F11Y02001,F11Y05034,F11Y05048) ->B

names(B) <- c("id","gender","edu","income","sat")
B
str(B$edu)
View(B$edu)

levels(B$edu)
