#Arima Model
install.packages(fpp2)
library(fpp2)
library(readxl)
library(tidyverse)

corona_kwt<- read_xlsx("corona-cases-kuwait.xlsx")
head(corona_kwt)
corona_ts_Pat<- ts(corona_kwt[,3]) #patients in bed
tail(corona_ts_Pat)
BoxCox.lambda(corona_ts_Pat)
arima_kw_Pat<- auto.arima(corona_ts_Pat, lambda = 0.342)%>%
  forecast(30)
autoplot(arima_kw_Pat)+
  xlab("Day")+
  ylab("Under Treatment")+
  ggtitle("Patients Under Treatment in Kuwait ARIMA forecast")


arima_kw_Pat

corona_tbats_pat<- tbats(corona_ts_Pat)%>% forecast(h=30)
corona_tbats_pat%>% autoplot()+ 
  xlab("Day Number")+ylab("Patients in Bed")+
  ggtitle("Corona KW Forecasts with my TBAT")+
  guides(color= guide_legend("Prediction interval"))

view(corona_tbats_pat)

install.packages("dygraphs")
library(dygraphs)
