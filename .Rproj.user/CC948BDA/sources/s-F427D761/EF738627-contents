# installing and loading readxl package 
install.packages("sqldf")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages('Rcpp')

library(readxl)
library(httr)
library(ggplot2)
library(sqldf)
library(dplyr)

#1. 데이터 셋 가져오기 - 연령대별 지지율
age=read_excel("9m_2w_age.xlsx")
str(age)

#회귀분석
model_age = lm(result~twen+thir+forty+fif+six+sev, data=age)
summary(model_age)

#2. 데이터 셋 가져오기 - 지역별 지지율
area=read_excel("9m2w_area.xlsx")
str(area)

#회귀분석
model_area = lm(result~seoul+kki+bwk+gwju, data=area)
summary(model_area)

#3. 데이터 셋 가져오기 - 이념성향별 지지율
think=read_excel("9m2w_think.xlsx")
str(think)

#회귀분석
model_think = lm(result~bosu+joong+jinbo+no, data=think)
summary(model_think)

#4. 데이터 셋 가져오기 - 직업별 지지율
job=read_excel("9m2w_job.xlsx")
str(job)

#회귀분석
model_job = lm(result~office_pro+service+mom+store+stu+rice_fish+mujik, data=job)
summary(model_job)


#5. 데이터 셋 가져오기 
# 연령대(20대+50대) + 지역(서울+경기+부산울산경남) + 이념성향(중도) + 직업(사무전문직+전업주부) 
multi=read_excel("9m2w_multi.xlsx")
str(multi)

#회귀분석
model_multi = lm(result~twen+seoul+kki+bwk+joong+office_pro, data=multi)
summary(model_multi)

#다중공선성 확인
install.packages('car')
library(car)
vif(model_multi)
