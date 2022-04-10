setwd("D:/Dropbox/MyDocuments/MD_4_1/4_R/과제2")
getwd()

# 데이터 불러오기
data <- read.csv("test20.csv")

## 전처리 ----------------------------------------------------------------------
# 첫번째 변수 한자명 수정
names(data)[1] <- 'sid'

# 결측치 처리
data[data<0] <- NA


## 변수 생성 -------------------------------------------------------------------

# 소규모 학교 
data$SCLASS <- ifelse(data$CLASS1<3, 1, 0)
# data$SCLASS = as.factor(data$SCLASS)

# 독서에 대한 즐거움(1학년, 2학년)
data$Y1READ <- rowSums(data[c("Y1S19_2","Y1S19_3", "Y1S19_5", "Y1S19_7")], na.rm=T)
data$Y2READ <- rowSums(data[c("Y2S14_2","Y2S14_3", "Y2S14_5", "Y2S14_7")], na.rm=T)


# 학교교육만족도(2학년)
data$Y2SES <- rowSums(data[c("Y2S5_15","Y2S5_16", "Y2S5_17", "Y2S5_18", "Y2S5_19", 
                              "Y2S5_20", "Y2S5_21", "Y2S5_22", "Y2S5_23")], na.rm=T)

# 학업자아개념(2학년)
data$Y2SSC <- rowSums(data[c("Y2S2_24","Y2S2_25", "Y2S2_26", "Y2S2_28", "Y2S2_29", 
                             "Y2S2_30")], na.rm=T)

# 부모학업지원(2학년)
data$Y2PSS <- rowSums(data[c("Y2S27_1","Y2S27_2", "Y2S27_3", "Y2S27_4", "Y2S27_5", 
                             "Y2S27_6", "Y2S27_7", "Y2S27_8")], na.rm=T)

# 부모정서지원(2학년)
data$Y2PES <- rowSums(data[c("Y2S27_9","Y2S27_10")], na.rm=T)


### 문제풀이 -------------------------------------------------------------------
attach(data)

## 문제 1 (카이제곱검정) ######################
# 패키지 설치
# install.packages("gmodels")
library(gmodels)

# 교육태도 1(선생님 학업성적 중요도)
hist(Y1S11_5)
hist(Y1S11_5, breaks=seq(0,5,by=1))
boxplot(Y1S11_5)
summary(Y1S11_5)
sd(Y1S11_5, na.rm=T)

# 교육태도 2(선생님 인성교육 중요도)
hist(Y1S11_6)
hist(Y1S11_6, breaks=seq(0,5,by=1))
boxplot(Y1S11_6)
summary(Y1S11_6)
sd(Y1S11_6, na.rm=T)

# 빈도표시
table(Y1S11_5,Y1S11_6)

# 카이제곱검정
# H0: 학업교육과 인성교육은 독립이다. H1: 학업교육과 인성교육은 독립이 아니다.
CrossTable(Y1S11_5,Y1S11_6, chisq = T,
           expected = T, dnn=c("사교육","학교만족도"),
           prop.r=F, prop.c=F, prop.t=F)
# p = 1.425563e-11 이므로 귀무가설을 기각. 따라서 두 변수는 독립이 아니고 연관이 있다.

# 범주 합치기
# 학업성적 중요도
table(Y1S11_5)
# Y1S11_5[is.na(Y1S11_5)==F & (Y1S11_5=="전혀 그렇지 않다" | Y1S11_5=="그
# 렇지 않다")] <- "그렇지 않다"
Y1S11_5[is.na(Y1S11_5)==F & (Y1S11_5==1 | Y1S11_5==2)] <- 2
table(Y1S11_5)

# 인성교육 중요도
table(Y1S11_6)
# Y1S11_6[is.na(Y1S11_6)==F & (Y1S11_6=="전혀 그렇지 않다" | Y1S11_6=="그
# 렇지 않다")] <- "그렇지 않다"
Y1S11_6[is.na(Y1S11_6)==F & (Y1S11_6==1 | Y1S11_6==2)] <- 2
table(Y1S11_6)

# 카이제곱검정
# H0: 학업교육과 인성교육은 독립이다. H1: 학업교육과 인성교육은 독립이 아니다.
CrossTable(Y1S11_5,Y1S11_6, chisq = T,
           expected = T, dnn=c("사교육","학교만족도"),
           prop.r=F, prop.c=F, prop.t=F)
# p = 5.544237e-13 이므로 귀무가설을 기각. 따라서 두 변수는 독립이 아니고 연관이 있다.
# 범주를 너무 나누면 범주형 변수 사이 비교가 힘들어서? 전혀! 그렇지 않다!
# 매우 그렇지 않다와 그렇지 않다가 표본의 수가 적어서



## 문제 2 (단순회귀분석) ######################
# 학교교육 만족도 
hist(Y2SES)
boxplot(Y2SES)
summary(Y2SES)
sd(Y2SES, na.rm=T)

# 학업성적 중요도에 따른 학교 만족도 회귀분석
model_SES_5 <- lm(Y2SES~Y1S11_5)
summary(model_SES_5)
# p-value가 0.9745이므로 회귀모델이 의미가 없다.


# 인성교육 중요도에 따른 학교 만족도 회귀분석
model_SES_6 <- lm(Y2SES~Y1S11_6)
summary(model_SES_6)
# p-value가 1.876e-05이므로 회귀모델이 의미가 있다.

# 회귀곡선 그리기
plot(Y2SES~Y1S11_6)
abline(model_SES_6, col='red')



## 문제 3 (상관분석) ######################
#중학교 2학년 국어성적 
hist(Y2KOR_S)
boxplot(Y2KOR_S)
summary(Y2KOR_S)
sd(Y2KOR_S, na.rm=T)

# 1. 학부모 최종학력, 월평균 가구소득, 부모의 학업지원 및 정서지원

cor(Y2KOR_S, PEDU, use="complete.obs", method="pearson")
plot(Y2KOR_S, PEDU)
abline(lm(Y2KOR_S~PEDU), col='red')

cor(Y2KOR_S, MEDU, use="complete.obs", method="pearson")
plot(Y2KOR_S, MEDU)
abline(lm(Y2KOR_S~MEDU), col='red')

cor(Y2KOR_S, INCOME, use="complete.obs", method="pearson")
plot(Y2KOR_S, INCOME)
abline(lm(Y2KOR_S~INCOME), col='red')

cor(Y2KOR_S, Y2PSS, use="complete.obs", method="pearson")
plot(Y2KOR_S, Y2PSS)
abline(lm(Y2KOR_S~Y2PSS), col='red')

cor(Y2KOR_S, Y2PES, use="complete.obs", method="pearson")
plot(Y2KOR_S, Y2PES)
abline(lm(Y2KOR_S~Y2PES), col='red')

# 2. 지역 규모, 설립구분, 남녀공학, 소규모학교
cor(Y2KOR_S, REGION, use="complete.obs", method="pearson")
plot(Y2KOR_S, REGION)
abline(lm(Y2KOR_S~REGION), col='red')

cor(Y2KOR_S, SECTOR, use="complete.obs", method="pearson")
plot(Y2KOR_S, SECTOR)
abline(lm(Y2KOR_S~SECTOR), col='red')

cor(Y2KOR_S, COEDU, use="complete.obs", method="pearson")
plot(Y2KOR_S, COEDU)
abline(lm(Y2KOR_S~COEDU), col='red')

cor(Y2KOR_S, SCLASS, use="complete.obs", method="pearson")
plot(Y2KOR_S, SCLASS)
abline(lm(Y2KOR_S~SCLASS), col='red')

# 3. 성별, 2학년 독서즐거움, 학업자아개념, 사교육참여, 1학년 국어성적
cor(Y2KOR_S, GENDER, use="complete.obs", method="pearson")
plot(Y2KOR_S, GENDER)
abline(lm(Y2KOR_S~GENDER), col='red')

cor(Y2KOR_S, Y2READ, use="complete.obs", method="pearson")
plot(Y2KOR_S, Y2READ)
abline(lm(Y2KOR_S~Y2READ), col='red')

cor(Y2KOR_S, Y2SSC, use="complete.obs", method="pearson")
plot(Y2KOR_S, Y2SSC)
abline(lm(Y2KOR_S~Y2SSC), col='red')

cor(Y2KOR_S, Y2P31_1, use="complete.obs", method="pearson")
plot(Y2KOR_S, Y2P31_1)
abline(lm(Y2KOR_S~Y2P31_1), col='red')

cor(Y2KOR_S, Y1KOR_S, use="complete.obs", method="pearson")
plot(Y2KOR_S, Y1KOR_S)
abline(lm(Y2KOR_S~Y1KOR_S), col='red')


## 문제 4 (중다회귀분석) ######################
# 패키지 설치
# install.packages("car")
library(car)

# 설정기준: 각 영영에서 가장 절대값이 높은 상관계수를 가지는 변수를 대표 변수로 설정
# 1. 어머니 최종학력
cor(Y2KOR_S, MEDU, use="complete.obs", method="pearson")
plot(Y2KOR_S, MEDU)

# 2. 소규모 학교
cor(Y2KOR_S, SCLASS, use="complete.obs", method="pearson")
plot(Y2KOR_S, SCLASS)

# 3. 1학년 국어성적
cor(Y2KOR_S, Y1KOR_S, use="complete.obs", method="pearson")
plot(Y2KOR_S, Y1KOR_S)

model_KOR_MSY <- lm(Y2KOR_S~MEDU+SCLASS+Y1KOR_S)
summary(model_KOR_MSY)
# MEDU와 SCLAss는 유의확률이 0.134, 0.410으로 0.05보다 크므로 유의하지 않다.
vif(model_KOR_MSY)
# vif값이 10을 넘지 않으므로 다중공선성 문제는 없는 것으로 보인다.
tail(sort(cooks.distance(model_KOR_MSY)))
tail(sort(hatvalues(model_KOR_MSY)))
# tail값이 1을 넘지 않으므로 이상치는 없는 것으로 보인다.

model_KOR_MSY2 <- lm(Y2KOR_S~MEDU*SCLASS*Y1KOR_S)
summary(model_KOR_MSY2)


## 문제 5 (단순회귀분석) ######################

# 1학년 국어성적과 2학년 국어성적 사이의 상관계수가 높다. 
# 이를 통해 최적의 모형 도출 가능
# 문제 4에서 보았듯이 중다회귀분석에서 MEDU와 SCLAss는 유의확률이 0.134, 0.410으로 
# 0.05보다 크므로 유의하지 않다. 따라서 Y1KOR_S로만 분석하여 최적의 모델을 만든다.
cor(Y2KOR_S, Y1KOR_S, use="complete.obs", method="pearson")
plot(Y2KOR_S, Y1KOR_S)
model_KOR_21 <- lm(Y2KOR_S~Y1KOR_S)
abline(model_KOR_21, col='red')

