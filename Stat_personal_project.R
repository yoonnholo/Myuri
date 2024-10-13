Raw_data <- read_csv("~/Downloads/KCYPS2018m1[CSV]/KCYPS2018m1[CSV]/KCYPS2018m1Yw5.csv")


table(KCYPS2018m1Yw5$YTIM1I01w5)
table(KCYPS2018m1Yw5$YTIM1I02w5)
table(KCYPS2018m1Yw5$YPSY6Ascw5)

New_data <- data.frame(
  YTIM1I01w5 = Raw_data$YTIM1I01w5,
  YTIM1I02w5 = Raw_data$YTIM1I02w5
)

New_data$Total_book <- New_data$YTIM1I01w5 + New_data$YTIM1I02w5
table(New_data$Total_book)

New_data$Book_Group <- ifelse(New_data$Total_book == 2, "A",
                              ifelse(New_data$Total_book >= 3 & New_data$Total_book <= 4, "B",
                                     ifelse(New_data$Total_book >= 5, "C", NA)))
## A - 전혀 안 읽음, B - 읽음

New_data$Case01 <- Raw_data$YPSY1A01w5 + Raw_data$YPSY1A02w5 + Raw_data$YPSY1A03w5 + Raw_data$YPSY1A04w5 + Raw_data$YPSY1A05w5

New_data$School_type <- Raw_data$SCLTYPw5
table(New_data$School_type)

chisq_test <- chisq.test(table(New_data$Book_Group, New_data$School_type))
print(chisq_test)







# Book_Group과 School_type_merged 간의 교차표 생성
cross_table <- table(New_data$Book_Group, New_data$School_type)

# 교차표에 합계를 추가
cross_table_with_margins <- addmargins(cross_table)

# 카이제곱 검정 수행
chisq_test <- chisq.test(cross_table)

# 교차표와 카이제곱 검정 결과 출력
print("교차표 (Book_Group과 School_type):")
print(cross_table_with_margins)

print("카이제곱 검정 결과:")
print(chisq_test)


New_data <- New_data[New_data$School_type != 7, ]
New_data$School_type[New_data$School_type == 5] <- 6
