file_path <- "satis.dat"
satis_df <- read.table(file_path, header=TRUE)

file_path <- "attract.dat"
attract_df <- read.table(file_path, header=TRUE)


table(attract_df$face)

ggplot(attract_df, aes(x = face, y = attract)) +
  geom_point() +  
  geom_smooth(method = "lm", se = TRUE, level = 0.99, color = "red") +  
  labs(title = "Scatter Plot with Regression Line for Attractiveness and Face",
       x = "Face", y = "Attractiveness") +
  theme_minimal()

ggplot(attract_df, aes(x = face, y = attract, color = sex)) +
  geom_point() +  
  labs(title = "Scatter Plot for Attractiveness and Humor by Sex",
       x = "Face", y = "Attractiveness") +
  theme_minimal()

ggplot(subset(attract_df, sex == "M"), aes(x = factor(sex), y = iqscore)) +
  geom_boxplot() +  
  labs(title = "Box Plot for IQ Score (Sex: M)",
       x = "Sex", y = "IQ Score") +
  theme_minimal()


