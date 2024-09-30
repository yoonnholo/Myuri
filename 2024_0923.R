mat <- matrix(c(3:14), nrow = 4, byrow = T)

data <- c("1", "2", "3", "4")
Fac <- as.numeric(data)

d <- c(1, 2, 3, 4)
e <- c("red", "white", "skyblue", NA)
f <- c(T, T, F, T)

df <- data.frame(d, e, f)
names(df) <- c("ID", "Color", "S/U")


## 소지품 ID, 색깔, 가격

ID <- c(1, 2, 3)
색깔 <- c("orange", "blue", "purple")
가격 <- c(500000, 30000, 1000)
이름 <- c("아이폰", "잔스포츠배낭", "CU1리터생수")

my_df <- data.frame(ID, 색깔, 가격, 이름)




comma_value <- read.table("~/Desktop/무제.txt", header = T, sep = ",")
