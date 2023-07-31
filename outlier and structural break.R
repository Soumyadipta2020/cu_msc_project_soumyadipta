library("tseries")
library("strucchange")
library("forecast")
library("tsoutliers")

data=read.csv(file.choose())
y=ts(data[,2], frequency=12, start=c(1994,4))
plot.ts(y)

#-----------finding time series outliers
tsoutliers(y)

tso(y,types=c("IO"))
tso(y,types=c("AO"))
tso(y,types=c("LS"))
tso(y,types=c("TC"))

tso(y,types=c("IO","AO","LS","TC"))

#---------Structural Breaks
bp_y = breakpoints(y~1)    #-----storing the break points
summary(bp_y)
ci_y=confint(bp_y)
plot(y)
lines(bp_y)
lines(ci_y)

y_1 <- data[264:310,2]
fit1 <- auto.arima(y_1, ic="aic", trace=TRUE)
forecast(fit1, lead=9)
