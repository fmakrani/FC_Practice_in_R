library(fpp2)
library(readxl)
library(tidyverse)

gold_month<- read_xlsx("gold_price.xlsx") # read excel
gold_ts<- ts(gold_month[,2], start = 1000) #start reading the time series from 1000 month

goldfc<- tbats(gold_ts)%>%forecast(h=36)
autoplot(goldfc)
goldfc

goldINR<- read_excel("goldINR.xlsx")
goldINR<- ts(goldINR[,2], start = 8000)
autoplot(goldINR)

goldfc<- tbats(goldINR)%>% forecast(h=3000)
autoplot(goldfc)+xlab("Days")+ylab("Price INR/Ounce")+ggtitle("Gold Price INR Forecast fore next 3000 Days")

