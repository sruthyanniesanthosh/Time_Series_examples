##Homework Assignment 4 - Sruthy Annie Santhosh, 312213
##set the directory and importing the required libraries

setwd("/Users/sruthysanthosh/Desktop/Sem2")

library("timeSeries")

## (1) Exercise 1 : Identify each of the following as specific ARIMA models and
##state whether or not they are stationary. (In each case {Zt} denotes white
##                                           noise.)

## a) = Xt−1 + Zt − Zt−1 + 0.25Zt−2.
## Finding the root of the polynomial function: Φ(x) = 1 - x 

polyroot(c(1, -1))

## Finding the root of the polynomial function:  Θ(x) = 1 - x + 0.25 x2

polyroot(c(1,-1,0.25))

## b) Xt = 1.3Xt−1 − 0.3Xt−2 + Zt .	
## Finding the root of the polynomial function: Φ(x) = 1 - 1.3x  + 0.3 x2
polyroot(c(1,-1.3,0.3))

## Finding the root of the polynomial function: Θ(x) = 1 
polyroot(c(1))

## c) Xt = 1.2Xt−1 − 0.36Xt−2 + Zt + 0.8Zt−1.
## Finding the root of the polynomial function: Φ(x) = 1 - 1.2x + 0.36 x2
polyroot(c(1,-1.2,0.36))

##Finding the root of the polynomial function: Θ(x) = 1 + 0.8x
polyroot(c(1,0.8))

## d) Xt = 0.4Xt−1 + 0.6Xt−2 + Zt − Zt−1 + 0.16Zt−2
## Finding the root of the polynomial function: Φ(x) = 1 - 0.4x + 0.6 x2
polyroot(c(1,-0.4,-0.6))

##Finding the root of the polynomial function: Θ(x) = 1 - x + 0.16 x2
polyroot(c(1, -1, 0.16))

### Exercise 2 
library(tseries)

#Reading the data
www <- "cbe.dat"
x <- read.table(www, header = T)
Elec <- ts(x[,3], start = c(1958,1), end = c(1990,12), freq = 12)
plot(Elec, type = "l", main = "Electricity production series from Jan 1958 to Dec 1990",
     xlab="Time from 1958 to 1990", ylab= "Monthly supply of electricity")

## Fitting a seasonal ARIMA model to the time series yt:= log xt
Elec.log <- log(Elec)
plot(Elec.log,main = "Log of Electricity production series from Jan 1958 to Dec 1990",
     xlab="Time from 1958 to 1990", ylab= "Log of Monthly supply of electricity")

## 1) With D = 1, choose d using the KPSS test for (non)-stationarity
kpss.test(diff(Elec.log, lag=12))

## 2)Choosing p,q,P Q ∈ {0, 1} according to the best AIC for the logarithm of 
##the original series

AIC(arima(Elec.log, order = c(0,1,0), seas = list(order = c(0,1,0), 12)))
AIC(arima(Elec.log, order = c(0,1,0), seas = list(order = c(0,1,1), 12)))
AIC(arima(Elec.log, order = c(0,1,0), seas = list(order = c(1,1,0), 12)))
AIC(arima(Elec.log, order = c(0,1,0), seas = list(order = c(1,1,1), 12)))

AIC(arima(Elec.log, order = c(0,1,1), seas = list(order = c(0,1,0), 12)))
AIC(arima(Elec.log, order = c(0,1,1), seas = list(order = c(0,1,1), 12)))
AIC(arima(Elec.log, order = c(0,1,1), seas = list(order = c(1,1,0), 12)))
AIC(arima(Elec.log, order = c(0,1,1), seas = list(order = c(1,1,1), 12)))

AIC(arima(Elec.log, order = c(1,1,0), seas = list(order = c(0,1,0), 12)))
AIC(arima(Elec.log, order = c(1,1,0), seas = list(order = c(0,1,1), 12)))
AIC(arima(Elec.log, order = c(1,1,0), seas = list(order = c(1,1,0), 12)))
AIC(arima(Elec.log, order = c(1,1,0), seas = list(order = c(1,1,1), 12)))

AIC(arima(Elec.log, order = c(1,1,1), seas = list(order = c(0,1,0), 12)))
AIC(arima(Elec.log, order = c(1,1,1), seas = list(order = c(0,1,1), 12)))
AIC(arima(Elec.log, order = c(1,1,1), seas = list(order = c(1,1,0), 12)))
AIC(arima(Elec.log, order = c(1,1,1), seas = list(order = c(1,1,1), 12)))

## Best AIC value
arima(Elec.log, order = c(0,1,1),
      seas = list(order = c(1,1,1), 12))

## 3. Plot the correlogram of the residuals of the best fitted ARIMA process.

y.arima <- arima(Elec.log, order = c(0,1,1),
                  seas = list(order = c(1,1,1), 12))
plot(acf(resid(y.arima)), main = "Correlogram of the residuals of the best 
fitted ARIMA process")

