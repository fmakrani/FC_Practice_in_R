#Arima Model
install.packages(fpp2)
library(fpp2)
library(readxl)
library(tidyverse)

corona_kwt<- read_xlsx("corona-cases-kuwait.xlsx")
head(corona_kwt)
corona_ts<- ts(corona_kwt[,2])
autoplot(corona_ts)

arima_kw<- auto.arima(corona_ts, lambda = 0.2433)%>%
  forecast(h=30)

checkresiduals(arima_kw)

autoplot(arima_kw)+
  xlab("Day")+
  ylab("No of Cases")+
  ggtitle("Daily No of Cases")

#arima_kw

ggAcf(corona_ts)
