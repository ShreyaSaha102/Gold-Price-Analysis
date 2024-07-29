rm(list=ls())
data=read.csv("C:/Users/SHREYA SAHA/Documents/datasets/gold_price_monthly.csv",header=T)
data
attach(data)

#time series object
ts.1=ts(data$Price,start=c(1950,1),frequency=12)
ts.1
plot.ts(ts.1)

#Dividing the data inot train & test
N=length(ts.1);N
n=0.8*N;n
train=data[1:n,];train
test=data[(n+1):N,];test

#train data
train.ts=ts(train$Price,start=c(1950,1),frequency=12);train.ts
plot(train.ts)

#Augmented Dickey-Fuller
library(tseries)
adf.test(train.ts)

#differencing
diff1=diff(train.ts,differences=1)
adf.test(diff1)

#acf&pacf
library(forecast)
acf=acf(na.omit(diff1),lag.max=100,type=c("correlation"),plot=T,main="ACF Plot")
pacf=pacf(na.omit(diff1),lag.max=100,plot=T,main="PACF Plot")

#arima
fit=arima(diff1,order=c(4,1,3),include.mean=T)
fit

#test ts
test.ts=ts(test$Price,start=c(2006,1),frequency=12)
diff.test=diff(test.ts,differences=1)
df1=data.frame(diff.test);df1
length(test$Price)

#RMSE
pred=forecast(fit,h=168)
df=data.frame(pred)
y_pred=ts(df$Point.Forecast,start=c(2006,1),frequency=12);y_pred
y_true=df1$diff.test
rmse=sqrt(mean((y_pred-y_true)^2));rmse


#=================SARIMA=================

acf=acf(na.omit(train.ts),lag.max=300,type=c("correlation"),plot=T,main="ACF Plot")
pacf=pacf(na.omit(train.ts),lag.max=300,plot=T,main="PACF Plot")
fit1=arima(diff1,order=c(4,1,3),seasonal=list(order=c(1,1,0),period=12))
fit1

#test ts
test.ts=ts(test$Price,start=c(2006,1),frequency=12)
diff.test=diff(test.ts,differences=1)
df1=data.frame(diff.test);df1

#RMSE
pred=forecast(fit1,h=168)
df=data.frame(pred)
y_pred=ts(df$Point.Forecast,start=c(2006,1),frequency=12);y_pred
y_true=df1$diff.test
rmse=sqrt(mean((y_pred-y_true)^2));rmse



