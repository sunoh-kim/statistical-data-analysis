# 경로 확인 (컴퓨터 환경설정마다 다름)
# setwd("D:/Dropbox/MyDocuments/MD_4_1/4_R/과제1")
setwd("C:/Users/pil/Dropbox/MyDocuments/MD_4_1/4_R/과제1")
getwd()

# 데이터 불러오기
data <- read.csv("test20.csv")

## 전처리 ----------------------------------------------------------------------
# 첫번째 변수 한자명 수정
names(data)[1] <- 'sid'

# 결측치 처리
names_for_NA <- c("Y1S19_2","Y1S19_3", "Y1S19_5", 
                  "Y1S19_7", "Y2S14_2","Y2S14_3", "Y2S14_5", "Y2S14_7", "Y2S5_15",
                  "Y2S5_16", "Y2S5_17", "Y2S5_18", "Y2S5_19", "Y2S5_20", "Y2S5_21", 
                  "Y2S5_22", "Y2S5_23", "Y2S2_24","Y2S2_25", "Y2S2_26", "Y2S2_28", 
                  "Y2S2_29", "Y2S2_30", "Y2S27_1", "Y2S27_2", "Y2S27_3", "Y2S27_4", 
                  "Y2S27_5", "Y2S27_6", "Y2S27_7", "Y2S27_8", "Y2S27_9", "Y2S27_10")
for(name_for_NA in names_for_NA){
  data[name_for_NA][data[name_for_NA] <0] <- NA
}


# 데이터 타입 변경
data$GENDER = as.factor(data$GENDER)
data$PEDU = as.factor(data$PEDU)
data$MEDU = as.factor(data$MEDU)
data$REGION = as.factor(data$REGION)
data$SECTOR = as.factor(data$SECTOR)
data$COEDU = as.factor(data$COEDU)
data$Y1S15 = as.factor(data$Y1S15)
data$Y1S11_5 = as.factor(data$Y1S11_5)
data$Y1S11_6 = as.factor(data$Y1S11_6)

# 레벨 설정
levels(data$GENDER) <- c("남성", "여성")
levels(data$REGION) <- c("특별시", "광역시", "중소도시", "읍면지역")
levels(data$SECTOR) <- c("국공립", "사립")
levels(data$COEDU) <- c("남녀공학", "남학교", "여학교")
levels(data$Y1S15) <- c("무응답", "있다", "없다")
levels(data$Y1S11_5) <- c("무응답", "전혀그렇지 않다", "그렇지 않다", "보통이다", "그렇다", "매우 그렇다")
levels(data$Y1S11_6) <- c("무응답", "전혀그렇지 않다", "그렇지 않다", "보통이다", "그렇다", "매우 그렇다")


## 변수 생성 -------------------------------------------------------------------

# 소규모 학교 
data$SCLASS <- ifelse(data$CLASS1<3, 1, 0)
data$SCLASS = as.factor(data$SCLASS)

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

## 문제 1 (두 종속표본 t검증) ######################

#hist(data$Y1READ)
#boxplot(data$Y1READ)
# plot(data$Y1READ)
#summary(data$Y1READ)

#hist(data$Y2READ)
#boxplot(data$Y2READ)
# plot(data$Y2READ)
#summary(data$Y2READ)

# 독서에 대한 즐거움 t검정
model_READ <- t.test(data$Y1READ, data$Y2READ, paired = TRUE)
print(model_READ)
print("1,2학년 때 학생들의 독서에 대한 즐거움 정도에 차이가 있다.")

# 국어 성적 t검정
model_KOR <- t.test(data$Y1KOR_S, data$Y2KOR_S, paired = TRUE)
print(model_KOR) # => 차이 존재
print("1,2학년 때 학생들의 국어 성적에 차이가 있다.")


## 문제 2 (두 독립표본 t검증) ######################

# 소규모 여부 등분산 검정
var.test(Y2SES~SCLASS, data) # 두 분산이 같다. 
var.test(Y2SSC~SCLASS, data) # 두 분산이 같다. 
var.test(Y2KOR_S~SCLASS, data) # 두 분산이 같다. 
# install.packages("car")
library("car")
leveneTest(Y2SES~SCLASS, data) # 두 분산이 같다.
leveneTest(Y2SSC~SCLASS, data) # 두 분산이 같다. 
leveneTest(Y2KOR_S~SCLASS, data) # 두 분산이 같다. 

# 성별 등분산 검정
var.test(Y2SES~GENDER, data) # 두 분산이 다르다 
var.test(Y2SSC~GENDER, data) # 두 분산이 다르다 
var.test(Y2KOR_S~GENDER, data) # 두 분산이 같다. 

leveneTest(Y2SES~GENDER, data) # 두 분산이 같다.
leveneTest(Y2SSC~GENDER, data) # 두 분산이 같다. 
leveneTest(Y2KOR_S~GENDER, data) # 두 분산이 같다. 

# 소규모 여부 t검정
model_S_SES <- t.test(Y2SES~SCLASS, data, var.equal=T)
model_S_SSC <- t.test(Y2SSC~SCLASS, data, var.equal=T)
model_S_KOR <- t.test(Y2KOR_S~SCLASS, data, var.equal=T)
print(model_S_SES)
print("학교가 소규모 학교인지 여부에 따라서 학교교육 만족도에 차이가 있다.")
print(model_S_SSC)
print("학교가 소규모 학교인지 여부에 따라서 학업자아개념에 차이가 없다.")
print(model_S_KOR)
print("학교가 소규모 학교인지 여부에 따라서 국어 성적에 차이가 있다.")

# 성별 t검정
model_G_SES <- t.test(Y2SES~GENDER, data, var.equal=T)
model_G_SSC <- t.test(Y2SSC~GENDER, data, var.equal=T)
model_G_KOR <- t.test(Y2KOR_S~GENDER, data, var.equal=T)
print(model_G_SES)
print("성별에 따라서 학교교육 만족도에 차이가 없다.")
print(model_G_SSC)
print("성별에 따라서 학업자아개념에 차이가 없다.")
print(model_G_KOR)
print("성별에 따라서 국어 성적에 차이가 있다.")


## 문제 3 (일원분산분석) ######################

# 남녀공학 여부 등분산 검정
leveneTest(Y2SES~COEDU, data) # 두 분산이 다르다.

# 지역규모 등분산 검정
leveneTest(Y2SES~REGION, data) # 두 분산이 같다.

# 남녀공학 여부 분산분석
model_C_SES <- aov(Y2SES~COEDU, data)
summary(model_C_SES)
print("학교가 남녀공학인지 아니면 남학교 혹은 여학교인지에 따라서 학생들의
학교교육 만족도에 차이가 없다.")

# 남녀공학 여부 사후검정
tukey_C_SES <- TukeyHSD(model_C_SES)
print(tukey_C_SES)
plot(tukey_C_SES)
print("모든 집단이 0을 포함하기 때문에 모든 집단의 평균은 같다고 볼 수 있다.")

# 지역규모 분산분석
model_R_SES <- aov(Y2SES~REGION, data)
summary(model_R_SES)
print("지역규모에 따라서 학교교육 만족도에 차이가 없다.")

# 지역규모 사후검정
tukey_R_SES <- TukeyHSD(model_R_SES)
print(tukey_R_SES)
plot(tukey_R_SES)
print("모든 집단이 0을 포함하기 때문에 모든 집단의 평균은 같다고 볼 수 있다.")


## 문제 4 (이원분산분석) ######################

# 지역규모, 학생회활동경험 분산분석
model_RY_SES <- aov(Y2SES~REGION*Y1S15, data)
summary(model_RY_SES)
print("REGION:Y1S15에 차이가 없으니 교호작용은 무시한다.")

model_RY_SES2 <- aov(Y2SES~REGION+Y1S15, data)
summary(model_RY_SES2)
print("지역규모, 학생회활동경험에 따라서 학교교육 만족도에 차이가 없다.")

# 지역규모, 학생회활동경험 사후검정
tukey_RY_SES <- TukeyHSD(model_RY_SES2)
print(tukey_RY_SES)
plot(tukey_RY_SES)
print("모든 집단이 0을 포함하기 때문에 모든 집단의 평균은 같다고 볼 수 있다.")

interaction.plot(x.factor=data$REGION,trace.factor=data$Y1S15,
                 response=data$Y2SES, fun=mean, type="b", lwd = 3,
                 pch=c(2,3,4),col=c(2,3,4),xlab="지역규모",ylab="학교교육 만족도")


## 문제 5 (이원분산분석) ######################

# 설립구분, 지역규모 분산분석
model_SR_Y1H7 <- aov(Y1H7_2_1~SECTOR*REGION, data)
summary(model_SR_Y1H7)
print("SECTOR:REGION에 차이가 있으니 교호작용은 고려한다.")
# print("설립구분, 지역규모에 따라서 수준별 이동수업을 하는지 여부에 차이가 있다.")

# 설립구분, 지역규모 사후검정
tukey_SR_Y1H7 <- TukeyHSD(model_SR_Y1H7)
print(tukey_SR_Y1H7)
plot(tukey_SR_Y1H7)
# print("모든 집단이 0을 포함하기 때문에 모든 집단의 평균은 같다고 볼 수 있다.")

interaction.plot(x.factor=data$SECTOR, trace.factor=data$REGION,
                 response=data$Y1H7_2_1, fun=mean, type="b", lwd = 3,
                 pch=c(2,3,4,5),col=c(2,3,4,5),xlab="설립구분",ylab="이동수업여부")


## 문제 6 (이원분산분석) ######################

# 선생님이 생각하는 학업성적 중요도, 인성교육 중요도 분산분석
model_YY_Y1MAT <- aov(Y1MAT_S~Y1S11_5*Y1S11_6, data)
summary(model_YY_Y1MAT)
print("Y1S11_5:Y1S11_6에 차이가 없으니 교호작용은 고려하지 않다.")

model_YY_Y1MAT2 <- aov(Y1MAT_S~Y1S11_5+Y1S11_6, data)
summary(model_YY_Y1MAT2)
# print("선생님이 생각하는 학업성적 중요도, 인성교육 중요도에 따라서 수준별 이동수업을 하는지 여부에 차이가 있다.")

# 선생님이 생각하는 학업성적 중요도, 인성교육 중요도 사후검정
tukey_YY_Y1MAT <- TukeyHSD(model_YY_Y1MAT2)
print(tukey_YY_Y1MAT)
plot(tukey_YY_Y1MAT)
print("모든 집단이 0을 포함하기 때문에 모든 집단의 평균은 같다고 볼 수 있다.")

interaction.plot(x.factor=data$Y1S11_5,trace.factor=data$Y1S11_6,
                 response=data$Y1MAT_S, fun=mean, type="b", lwd = 3,
                 pch=c(2,3,4,5,6,7),col=c(2,3,4,5,6,7),xlab="선생님의 학업성적 중요도",ylab="수학성적")

