## sononmum logistic ##

df <- read.csv("KCYPS2018m1Yw4.csv")

## 종속변수: YDLQ2A01w4
## 독립변수: YTIM1L01w4, YTIM1L02w4
## 통제변수: YGENDERw4, YDLQ1A15w4, YPSY4B01w4:YPSY4B06w4

new_df <- select(df, YDLQ2A01w4, YTIM1L01w4, YTIM1L02w4, YGENDERw4, YDLQ1A15w4, YPSY4B01w4:YPSY4B06w4)

new_df <- new_df %>%
  rename(
    cyber_abuse = YDLQ2A01w4,
    weekday_computer = YTIM1L01w4,
    weekend_computer = YTIM1L02w4,
    gender = YGENDERw4,
    real_abuse = YDLQ1A15w4)

new_df <- new_df %>%
  mutate(aggression = rowSums(select(., YPSY4B01w4:YPSY4B06w4)))

new_df <- new_df %>%
  select(-YPSY4B01w4, -YPSY4B02w4, -YPSY4B03w4, -YPSY4B04w4, -YPSY4B05w4, -YPSY4B06w4)

na_count <- colSums(is.na(df_cleaned))

df_cleaned <- na.omit(new_df)

table(df_cleaned$weekend_computer)

df_cleaned <- df_cleaned %>%
  mutate(cyber_abuse = ifelse(cyber_abuse == 1, 0, 1))
df_cleaned <- df_cleaned %>%
  mutate(real_abuse = ifelse(real_abuse == 1, 0, 1))
df_cleaned <- df_cleaned %>%
  mutate(gender = ifelse(gender == 2, 0, gender))
df_cleaned <- df_cleaned %>%
  mutate(weekday_computer = case_when(
    weekday_computer %in% c(1, 2, 3) ~ "L",
    weekday_computer %in% c(4, 5) ~ "M",
    weekday_computer %in% c(6, 7) ~ "H",
    TRUE ~ as.character(weekday_computer)
  ))

table(df_cleaned$real_abuse)
prop.table(table(df_cleaned$real_abuse))
var(df_cleaned$aggression)

dummy_vars <- model.matrix(~ weekend_computer - 1, data = df_cleaned)
dummy_df <- as.data.frame(dummy_vars)
df_dummy <- cbind(df_dummy, dummy_df)

df_dummy <- df_dummy %>%
  select(-weekday_computer, -weekend_computer, -weekday_computerL, -weekend_computerL)



dependent_var <- "cyber_abuse"
independent_vars <- c("weekday_computerH", "weekday_computerM", "weekend_computerH", "weekend_computerM", "gender", "real_abuse", "aggression")

logistic_model <- glm(
  formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "))),
  data = df_dummy,
  family = "binomial"
)
summary(logistic_model)





vif_values <- car::vif(logistic_model)
print(vif_values)
dw_test <- durbinWatsonTest(logistic_model)
print(dw_test)

cor_matrix <- cor(df_dummy)
print(cor_matrix)
corrplot(cor_matrix, method = "color")


dependent_var <- "cyber_abuse"
independent_vars <- c("gender", "real_abuse", "aggression")

logistic_model <- glm(
  formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "))),
  data = df_dummy,
  family = "binomial"
)

summary(logistic_model)

exp(0.0648)
