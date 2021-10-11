install.packages("sqldf")
install.packages("dplyr")
install.packages("ggplot2")
library(sqldf)
library(dplyr)
library(ggplot2)

weather_day <- read.csv("weather1819.csv")

buy2018_libbam <- read.csv("buy2018_libbam.csv",fileEncoding = "UCS-2LE")
buy2018_pouder <- read.csv("buy2018_pouder.csv",fileEncoding = "UCS-2LE")
buy2018_nailcare <- read.csv("buy2018_nailcare.csv",fileEncoding = "UCS-2LE")
buy2018_wkcare <- read.csv("buy2018_wkcare.csv",fileEncoding = "UCS-2LE")
buy2018_eyecare <- read.csv("buy2018_eyecare.csv",fileEncoding = "UCS-2LE")
buy2018_bosup <- read.csv("buy2018_bosup.csv",fileEncoding = "UCS-2LE")
buy2018_trbcare <- read.csv("buy2018_trbcare.csv",fileEncoding = "UCS-2LE")
sunvim <- read.csv("sunvim.csv", header=T)
dust <- read.csv("dust.csv", header=T)
buy_all_hs <- read.csv("buy_all_hs.csv",fileEncoding = "UCS-2LE")

weather_day <- left_join(weather_day,sunvim,by='date')
weather_day <- left_join(weather_day,dust,by='date')
weather_day$dust[is.na(weather_day$dust)] <- 0


########################
# 립밤 휘귀식 -> ok(temp+dust)
#####################
buy2018_libbam_w <- left_join(buy2018_libbam,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_libbam_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_libbam_w))

summary(lm(sum_qty~temp+dust, data=buy2018_libbam_w))

########################
# 파우더  휘귀식
#####################
buy2018_pouder_w <- left_join(buy2018_pouder,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_pouder_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_pouder_w))

summary(lm(sum_qty~temp, data=buy2018_pouder_w))

########################
# buy2018_nailcare  휘귀식
#####################
buy2018_nailcare_w <- left_join(buy2018_nailcare,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_nailcare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_nailcare_w))

summary(lm(sum_qty~temp, data=buy2018_nailcare_w))

########################
# buy2018_wkcare  휘귀식
#####################
buy2018_wkcare_w <- left_join(buy2018_wkcare,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_wkcare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_wkcare_w))

summary(lm(sum_qty~temp, data=buy2018_wkcare_w))

########################
# buy2018_eyecare  휘귀식 -> ok(temp)
#####################
buy2018_eyecare_w <- left_join(buy2018_eyecare,weather_day,by='date')

lm(sum_qty~temp+rain, data=buy2018_eyecare_w)
summary(lm(sum_qty~temp+rain, data=buy2018_eyecare_w))

summary(lm(sum_qty~temp, data=buy2018_eyecare_w))

########################
# buy2018_bosup  휘귀식 -> ok(temp)
#####################
buy2018_bosup_w <- left_join(buy2018_bosup,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_bosup_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_bosup_w))

summary(lm(sum_qty~temp, data=buy2018_bosup_w))


########################
# buy2018_trbcare  휘귀식 
#####################
buy2018_trbcare_w <- left_join(buy2018_trbcare,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_trbcare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_trbcare_w))

summary(lm(sum_qty~temp, data=buy2018_trbcare_w))

####################################
# 2021.06.26. start
# reload
#####################################
library(sqldf)
library(dplyr)
library(ggplot2)

weather_day <- read.csv("weather1819.csv")
sunvim <- read.csv("sunvim.csv", header=T)
dust <- read.csv("dust.csv", header=T)

weather_day <- left_join(weather_day,sunvim,by='date')
weather_day <- left_join(weather_day,dust,by='date')
weather_day$dust[is.na(weather_day$dust)] <- 0


########################
# buy_all_hs  홍삼 휘귀식  X 
#####################
buy_all_hs <- read.csv("buy_all_hs.csv",fileEncoding = "UCS-2LE")
buy_all_hs_w <- left_join(buy_all_hs,weather_day,by='date')

b=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_hs_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_hs_w))

a=summary(lm(sum_qty~temp, data=buy_all_hs_w))
plot(b)



########################
# haircare(2018,2019, 2018+1029 3개 모두)  휘귀식 -> dust ok
#####################

buy2018_haircare <- read.csv("buy2018_haircare.csv",fileEncoding = "UCS-2LE")
buy2019_haircare <- read.csv("buy2019_haircare.csv",fileEncoding = "UCS-2LE")
buy_all_haircare <- read.csv("buy_all_haircare.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_haircare_w <- left_join(buy2018_haircare,weather_day,by='date')

s=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_haircare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_haircare_w))
plot(s)

summary(lm(sum_qty~temp, data=buy2018_haircare_w))

# 2019년도
buy2019_haircare_w <- left_join(buy2019_haircare,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_haircare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_haircare_w))

summary(lm(sum_qty~dust, data=buy2019_haircare_w))

# 2018+2019 모두
buy_all_haircare_w <- left_join(buy_all_haircare,weather_day,by='date')
s=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_haircare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_haircare_w))
plot(s)
summary(lm(sum_qty~dust, data=buy_all_haircare_w))

########################
# 클렌징 (2018,2019, 2018+1029 3개 모두)  휘귀식 -> 꽝
#####################

buy2018_clean <- read.csv("buy2018_clean.csv",fileEncoding = "UCS-2LE")
buy2019_clean <- read.csv("buy2019_clean.csv",fileEncoding = "UCS-2LE")
buy_all_clean <- read.csv("buy_all_clean.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_clean_w <- left_join(buy2018_clean,weather_day,by='date')

q=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_clean_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_clean_w))
plot(q)
summary(lm(sum_qty~temp, data=buy2018_clean_w))

# 2019년도
buy2019_clean_w <- left_join(buy2019_clean,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_clean_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_clean_w))

summary(lm(sum_qty~dust, data=buy2019_clean_w))

# 2018+2019 모두
buy_all_clean_w <- left_join(buy_all_clean,weather_day,by='date')
q=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_clean_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_clean_w))
plot(q)
summary(lm(sum_qty~dust, data=buy_all_clean_w))

########################
# 채소 (2018,2019, 2018+1029 3개 모두)  휘귀식 -> 2018년도 비 ok
#####################

buy2018_cheso <- read.csv("buy2018_cheso.csv",fileEncoding = "UCS-2LE")
buy2019_cheso <- read.csv("buy2019_cheso.csv",fileEncoding = "UCS-2LE")
buy_all_cheso <- read.csv("buy_all_cheso.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_cheso_w <- left_join(buy2018_cheso,weather_day,by='date')

a=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_cheso_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_cheso_w))
plot(a)
summary(lm(sum_qty~rain, data=buy2018_cheso_w))

# 2019년도
buy2019_cheso_w <- left_join(buy2019_cheso,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_cheso_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_cheso_w))

summary(lm(sum_qty~temp, data=buy2019_cheso_w))

# 2018+2019 모두
buy_all_cheso_w <- left_join(buy_all_cheso,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_cheso_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_cheso_w))

summary(lm(sum_qty~dust, data=buy_all_cheso_w))

########################
# 음료 (2018,2019, 2018+1029 3개 모두)  휘귀식 -> 온도+자외선 ok
#####################

buy2018_drink <- read.csv("buy2018_drink.csv",fileEncoding = "UCS-2LE")
buy2019_drink <- read.csv("buy2019_drink.csv",fileEncoding = "UCS-2LE")
buy_all_drink <- read.csv("buy_all_drink.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_drink_w <- left_join(buy2018_drink,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_drink_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_drink_w))

summary(lm(sum_qty~temp, data=buy2018_drink_w))

# 2019년도
buy2019_drink_w <- left_join(buy2019_drink,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_drink_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_drink_w))

summary(lm(sum_qty~temp+cum_sunvim, data=buy2019_drink_w))

# 2018+2019 모두
buy_all_drink_w <- left_join(buy_all_drink,weather_day,by='date')
a=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_drink_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_drink_w))
plot(a)
summary(lm(sum_qty~temp+cum_sunvim, data=buy_all_drink_w))


########################
# 습도조절 (2018,2019, 2018+1029 3개 모두)  휘귀식 -> 2018년도 temp+rain(온도, 비) ok
#####################

buy2018_mois <- read.csv("buy2018_mois.csv",fileEncoding = "UCS-2LE")
buy2019_mois <- read.csv("buy2019_mois.csv",fileEncoding = "UCS-2LE")
buy_all_mois <- read.csv("buy_all_mois.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_mois_w <- left_join(buy2018_mois,weather_day,by='date')

a=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_mois_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_mois_w))
plot(a)
summary(lm(sum_qty~temp+rain, data=buy2018_mois_w))

# 2019년도
buy2019_mois_w <- left_join(buy2019_mois,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_mois_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_mois_w))

summary(lm(sum_qty~temp+rain, data=buy2019_mois_w))

# 2018+2019 모두
buy_all_mois_w <- left_join(buy_all_mois,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_mois_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_mois_w))

summary(lm(sum_qty~temp+rain+cum_sunvim, data=buy_all_mois_w))

########################
# 썬케어 (2018,2019, 2018+1029 3개 모두)  휘귀식 -> temp+cum_sunvim+dust(온도, 자외선, 미세먼지) -> ok
#####################

buy2018_suncare <- read.csv("buy2018_suncare.csv",fileEncoding = "UCS-2LE")
buy2019_suncare <- read.csv("buy2019_suncare.csv",fileEncoding = "UCS-2LE")
buy_all_suncare <- read.csv("buy_all_suncare.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_suncare_w <- left_join(buy2018_suncare,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_suncare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_suncare_w))

summary(lm(sum_qty~temp+rain+cum_sunvim, data=buy2018_suncare_w))

# 2019년도
buy2019_suncare_w <- left_join(buy2019_suncare,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_suncare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_suncare_w))

summary(lm(sum_qty~temp+cum_sunvim+dust, data=buy2019_suncare_w))

# 2018+2019 모두
buy_all_suncare_w <- left_join(buy_all_suncare,weather_day,by='date')
a=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_suncare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_suncare_w))
plot(a)
summary(lm(sum_qty~temp+cum_sunvim+dust, data=buy_all_suncare_w))

########################
# 바디케어 (2018,2019, 2018+1029 3개 모두)  휘귀식 -> 온도
#####################

buy2018_bodycare <- read.csv("buy2018_bodycare.csv",fileEncoding = "UCS-2LE")
buy2019_bodycare <- read.csv("buy2019_bodycare.csv",fileEncoding = "UCS-2LE")
buy_all_bodycare <- read.csv("buy_all_bodycare.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_bodycare_w <- left_join(buy2018_bodycare,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_bodycare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_bodycare_w))

summary(lm(sum_qty~temp, data=buy2018_bodycare_w))

# 2019년도
buy2019_bodycare_w <- left_join(buy2019_bodycare,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_bodycare_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_bodycare_w))

summary(lm(sum_qty~temp, data=buy2019_bodycare_w))

# 2018+2019 모두
buy_all_bodycare_w <- left_join(buy_all_bodycare,weather_day,by='date')
a=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_bodycare_w)
plot(a)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_bodycare_w))

summary(lm(sum_qty~temp, data=buy_all_bodycare_w))

########################
# 난방 (2018,2019, 2018+1029 3개 모두)  -> 온도 자외선 미세먼지 -> ok
#####################

buy2018_heat <- read.csv("buy2018_heat.csv",fileEncoding = "UCS-2LE")
buy2019_heat <- read.csv("buy2019_heat.csv",fileEncoding = "UCS-2LE")
buy_all_heat <- read.csv("buy_all_heat.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_heat_w <- left_join(buy2018_heat,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_heat_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_heat_w))

summary(lm(sum_qty~temp, data=buy2018_heat_w))

# 2019년도
buy2019_heat_w <- left_join(buy2019_heat,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_heat_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_heat_w))

summary(lm(sum_qty~temp, data=buy2019_heat_w))

# 2018+2019 모두
buy_all_heat_w <- left_join(buy_all_heat,weather_day,by='date')
a=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_heat_w)
plot(a)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_heat_w))

summary(lm(sum_qty~temp+cum_sunvim+dust, data=buy_all_heat_w))

########################
# 공기청정기 (2018,2019, 2018+1029 3개 모두)  -> 온도+미세먼지(특히 미세먼지에) -> ok
#####################

buy2018_air <- read.csv("buy2018_air.csv",fileEncoding = "UCS-2LE")
buy2019_air <- read.csv("buy2019_air.csv",fileEncoding = "UCS-2LE")
buy_all_air <- read.csv("buy_all_air.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_air_w <- left_join(buy2018_air,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_air_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_air_w))

summary(lm(sum_qty~temp+dust, data=buy2018_air_w))

# 2019년도
buy2019_air_w <- left_join(buy2019_air,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_air_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_air_w))

summary(lm(sum_qty~dust, data=buy2019_air_w))

# 2018+2019 모두
buy_all_air_w <- left_join(buy_all_air,weather_day,by='date')
a=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_air_w)
plot(a)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_air_w))

summary(lm(sum_qty~temp+dust, data=buy_all_air_w))

########################
# 건강즙 (2018,2019, 2018+1029 3개 모두)  -> 온도+미세먼지 -> ok
#####################

buy2018_health <- read.csv("buy2018_health.csv",fileEncoding = "UCS-2LE")
buy2019_health <- read.csv("buy2019_health.csv",fileEncoding = "UCS-2LE")
buy_all_health <- read.csv("buy_all_health.csv",fileEncoding = "UCS-2LE")

# 2018년도
buy2018_health_w <- left_join(buy2018_health,weather_day,by='date')

lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_health_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2018_health_w))

summary(lm(sum_qty~temp+rain, data=buy2018_health_w))

# 2019년도
buy2019_health_w <- left_join(buy2019_health,weather_day,by='date')
lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_health_w)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy2019_health_w))

summary(lm(sum_qty~temp+dust, data=buy2019_health_w))

# 2018+2019 모두
buy_all_health_w <- left_join(buy_all_health,weather_day,by='date')
a=lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_health_w)
plot(a)
summary(lm(sum_qty~temp+rain+cum_sunvim+dust, data=buy_all_health_w))

summary(lm(sum_qty~temp+dust, data=buy_all_health_w))

