library(forecast)
library(tseries)
library(aTSA)
library(x12)
library(fGarch)

data = read.csv(file.choose())  #----reading data CSV file
head(data)
y_t = ts(data[,2],frequency=12,start=c(1994,4))
plot.ts(y_t, col="blue", ylab="WPI", main="WPI with base year 2011-12")

#--------Fitting linear trend
y=data[,2]
logy=log(y)
a=length(y)
x=c(1:a)
fity = lm(y~x)
fitlogy = lm(logy~x)
par(mfrow=c(2,1))
plot(x,y,type='l',xlab="time points", col="blue", ylab="WPI", main="WPI with base year 2011-12")
abline(fity, col="red")
plot(x,logy,type='l',xlab="time points", ylab="log(WPI)", col="blue", main="log(WPI) with base year 2011-12")
abline(fitlogy, col="red")

#-------------series after removing trend
y1 = resid(fity)
logy1 = resid(fitlogy)
par(mfrow=c(2,1))
plot(x,y1,type='l',xlab="time points", col="blue", ylab="WPI", main="WPI after trend")
plot(x,logy1,type='l',xlab="time points", col="blue", ylab="log(WPI)", main="log(WPI) after trend")

#-----calculating seasonal index
season_y <- c(0)
season_logy <- c(0)
for(i in 1:10){           #-----seasonal index from Apr to Dec, Jan
  s <- 0
  slog <- 0
  for(j in 1:26){
    s <- s+y1[i+(j-1)*12]
    slog <- slog+logy1[i+(j-1)*12]
  }
  if(i==10){
    season_y[1] <- s/26
    season_logy[1] <- slog/26
  }
  if(i!=10){
    season_y[i+3] <- s/26
    season_logy[i+3] <- slog/26
  }	
}

#----seasonal index for Feb, Mar
for(i in 11:12){           
  s <- 0
  slog <- 0
  for(j in 1:25){
    s <- s+y1[i+(j-1)*12]
    slog <- slog+logy1[i+(j-1)*12]
  }
  season_y[i-9] <- s/25
  season_logy[i-9] <- slog/25
}

#--------removing seasonal index
y2 = rep(0,times=310)
logy2 = rep(0,times=310)
for(i in 1:10)   #----loop for deseasonalization for Apr to Dec, Jan
{
  if(i!=10){
    for(j in 1:26)
    {
      y2[i+(j-1)*12] = y1[i+(j-1)*12] - season_y[i+3]
      logy2[i+(j-1)*12] = logy1[i+(j-1)*12] - season_logy[i+3]
    }
  }
  if(i==10){
    for(j in 1:26)
    {
      y2[i+(j-1)*12] = y1[i+(j-1)*12] - season_y[1]
      logy2[i+(j-1)*12] = logy1[i+(j-1)*12] - season_logy[1]
    }
  }
}

for(i in 11:12){          #------deseasonalization loop for Feb, Mar
  for(j in 1:25)
  {
    y2[i+(j-1)*12] = y1[i+(j-1)*12] - season_y[i-9]
    logy2[i+(j-1)*12] = logy1[i+(j-1)*12] - season_logy[i-9]
  }
}

par(mfrow=c(2,1))
plot(x,y2,type='l',xlab="time points", col="blue", ylab="WPI" ,main="WPI after detrended, deseasonalized")
plot(x,logy2,type='l',xlab="time points", col="blue", ylab="log(WPI)", main="log(WPI) after detrended, deseasonalized")

par(mfrow=c(2,2))
acf(y2)
acf(logy2)
pacf(y2)
pacf(logy2)


fit1=arima(y2,order=c(2,0,0))   #----fittinng MA(1)
fit2 = arima(logy2,order=c(2,0,0))    #-----fitting ARMA(2,3)
fit1
fit2

#--------diagonistic checking
arch.test(fit1) 
arch.test(fit2)

Box.test(resid(fit1),type="Ljung-Box")
Box.test(resid(fit2),type="Ljung-Box")

forecast(fit1, lead=9)
forecast(fit2, lead=9)
