library(tidyverse)

flight_df

flight_df %>% 
  filter(month==2) -> flight_df_2

flight_df %>% 
  filter(month==2) %>% 
  select(year:arr_delay) -> fly

fly %>% 
  mutate(result=arr_delay-dep_delay) %>% 
  mutate(final=ifelse(result<=0,"good","bad")) %>% 
  group_by(final) %>%
  summarise(MAX=max(result),
            MIN=min(result),
            MEAN=mean(result)) %>% 
  arrange(desc(final == "good"))


flight_df %>%              ##194342
  filter(arr_delay<=0) %>% 
  select(year:day,arr_time:arr_delay) %>% 
  