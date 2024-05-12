library(expsmooth)
library(forecast)
lag.plot(visitors, lags=4, do.lines=F)
trend=tslm(visitors~trend)
summary(trend)
prognoza=forecast(trend, h=4)
prognoza
plot(prognoza, main="Prognoza na podstawie trendu liniowego")
lines(trend$fitted.values, col="red", lty=3)

trend=tslm(bonds~trend)
summary(trend)
prognoza=forecast(trend, h=4)
prognoza
plot(prognoza, main="Prognoza na podstawie trendu liniowego")
lines(trend$fitted.values, col="red", lty=3)
