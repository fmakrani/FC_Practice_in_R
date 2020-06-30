library(fpp2)
library(tidyverse)
library(lubridate)
training<- window(corona_ts, end= 110)
test<- window(corona_ts, start= 110)
fc<- naive(training, h=17)
autoplot(fc)+
  autolayer(test, series = "Test Data")
checkresiduals(fc)
accuracy(fc)

e<- tsCV(corona_ts, forecastfunction = naive, h=1)
mean(e^2, na.rm = T)

sq<- function(u){u^2}
for(h in 1:10)
{
  corona_ts%>% tsCV(forecastfunction = naive, h=h)%>%
    sq()%>% mean(na.rm= T)%>% print
}
#MSE Mean Squired Error increases with forecast horizon

data_cor<- window(corona_ts, start = 99)
fc<- ses(data_cor, h= 5)
summary(fc)
autoplot(fc)+ ylab("No of Cases")+ xlab("Day Number")

corona_ts%>%holt(h=30)%>% autoplot()

fc1<-holt(corona_ts, h=30, PI= FALSE)
fc2<- holt(corona_ts, damped = T, h= 30, PI= F)
autoplot(corona_ts)+ xlab("Day Number")+ ylab("Cases")+
  autolayer(fc1, series = "Linear Trend")+
  autolayer(fc2, series = "Damped Trend")

#ETS Model
corona_ts%>% ets()%>% forecast(h=30) %>% 
  autoplot()+xlab("Day Number")+ylab("Corona Cases in Kuwait")+
  ggtitle("Corona KW Forecasts from ETS(A,N,N)")

#Transformations for variance stabilization
#Box-Cox transformations
BoxCox.lambda(corona_ts)

corona_ts %>% ets(lambda = 0.2433)%>% forecast(h=30)%>%
  autoplot()+xlab("Day Number")+ylab("Corona Cases in Kuwait")

#Dynamic harmonic regression
fit<- auto.arima(corona_ts, xreg = fourier(corona_kwt, K= 2, seasonal = F, lambda = 0))
#Not Successful

corona_tbats<-corona_ts%>% tbats()%>% forecast(h=100)
corona_tbats 
  autoplot()+xlab("Day Number")+ylab("Corona Cases in Kuwait")
