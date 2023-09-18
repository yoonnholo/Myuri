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
