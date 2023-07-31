library(forecast)
library(tseries)
library(aTSA)
library(fGarch)

#---------------- Reading the data
data = read.csv(file.choose())
y = data[,2]
logy = log(y)

#-------------- Reading dummy variables
dummy = read.csv(file.choose())
X = data.matrix(dummy[,2:13])

fity = lm(y~X-1)
fitlogy = lm(logy~X-1)

y1 = resid(fity)
logy1 = resid(fitlogy)

par(mfrow=c(2,1))
plot.ts(y1, col="blue", ylab="WPI", main="WPI after removing deterministic seasonality")
plot.ts(logy1, col="blue", ylab="log(WPI)", main="log(WPI) after removing deterministic seasonality")

ndiffs(y1)
ndiffs(logy1)

y2 <- diff(y1,differences=1)
logy2 <- diff(logy1,differences=1)

par(mfrow=c(2,2))
acf(y2)
pacf(y2)
acf(logy2)
pacf(logy2)

fit1=arima(y1, order=c(4,1,2))
fit2=arima(logy1, order=c(0,1,5))
fit1
fit2

par(mfrow=c(2,1))
plot.ts(resid(fit1))
plot.ts(resid(fit2))

Box.test(resid(fit1), type ="Ljung-Box")
Box.test(resid(fit2), type ="Ljung-Box")

pacf(resid(fit1)^2)
pacf(resid(fit2)^2)

arch.test(fit1)
arch.test(fit2)
