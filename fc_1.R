#Arima Model
install.packages(fpp2)
library(fpp2)
library(readxl)
library(tidyverse)

corona_kwt<- read_xlsx("corona-cases-kuwait.xlsx")
tail(corona_kwt)
corona_ts<- ts(corona_kwt[,2])
autoplot(corona_ts)
BoxCox.lambda(corona_ts) #check lambda
arima_kw<- auto.arima(corona_ts, lambda = 0.275571)%>%
  forecast(h=30)

checkresiduals(arima_kw)
arima_kw
autoplot(arima_kw)+
  xlab("Day")+
  ylab("No of Cases")+
  ggtitle("Daily No of Cases ARIMA Model No professional use")

#arima_kw

ggAcf(corona_ts)

tbats_kw<- tbats(corona_ts)%>% forecast(h=30)
autoplot(tbats_kw)+
  xlab("Day")+
  ylab("No of Cases")+
  ggtitle("Daily No of Cases TBATS Model")
  
tbats_kw
