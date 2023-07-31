library(forecast)
library(tseries)
library(aTSA)
library(x12)
library(fGarch)

data = read.csv(file.choose())  #----reading data CSV file
head(data)
y = ts(data[,2],frequency=12,start=c(1994,4))
logy = log(y)

par(mfrow=c(2,1))
plot.ts(y)
plot.ts(logy)

auto.arima(y,ic="aic",trace=TRUE)
auto.arima(logy,ic="aic",trace=TRUE)

ndiffs(y)
nsdiffs(y)
ndiffs(logy)
nsdiffs(logy)

y1=diff(y,differences=1)
logy1=diff(logy,differences=1)

par(mfrow=c(2,2))
acf(y1,lag.max=48)
pacf(y1,lag.max=48)
acf(logy1,lag.max=48)
pacf(logy1,lag.max=48)

fit1=arima(y,order=c(1,1,2),seasonal=c(1,0,1))
fit2=arima(logy,order=c(1,1,2),seasonal=c(1,0,0))
fit1
fit2

par(mfrow=c(2,1))
plot.ts(resid(fit1))
plot.ts(resid(fit2))

Box.test(resid(fit1),type="Ljung-Box")
Box.test(resid(fit2),type="Ljung-Box")
