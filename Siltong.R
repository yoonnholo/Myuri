## 실험통계학 진짜 주제 정해야 한다 ##

## 세대별 그룹 구분 -> 특정 변수에 대한 회귀분석 후 기울기 비교
## 그룹 간 차이 확인

RAW
Siltong <- RAW
Siltong$IsZen <- ifelse(Siltong$W22DQ02A >= 1995, 1, 0)
head(Siltong$IsZen)
