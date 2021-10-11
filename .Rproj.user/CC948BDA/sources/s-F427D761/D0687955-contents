# installing and loading readxl package 

install.packages("readxl")
install.packages('Rcpp')

library(readxl)
library(httr)
library(ggplot2)

# 데이터 셋 가져오기 

#update.packages()
#이낙연 지지율
sex_lny=read_excel("sex_lny.xlsx")

bp <-ggplot(sex_lny, aes(x="이낙연",y=지지율,fill=성별))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

#이재명 지지율
sex_ljm=read_excel("sex_ljm.xlsx")

bp <-ggplot(sex_ljm, aes(x="이재명",y=지지율,fill=성별))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

#윤석열 지지율
sex_ysr=read_excel("sex_ysr.xlsx")


bp <-ggplot(sex_ysr, aes(x="윤석열",y=지지율,fill=성별))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie


#이재명 연령대별지지율
age_ljm=read_excel("age_ljm.xlsx")

bp <-ggplot(age_ljm, aes(x="이재명",y=지지율,fill=연령대))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie
