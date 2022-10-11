##Final Exam - Sruthy Annie Santhosh, 312213
##set the directory and importing the required libraries

setwd("/Users/sruthysanthosh/Desktop/Sem2")

library("timeSeries")
library(tseries)
-------------------------------------------------------------------------------------
##Problem 1: Let Zt be a sequence of independent normal random variables, 
## each with mean 0 and variance σ 2 > 0

### (1) Which, if any, of the following processes are causal? 

###(a) Solutions of polynomial Φ(x) = 1 - ⅚ x + ⅙ x2

polyroot(c(1, -5/6, 1/6))

###(b) Solutions of polynomial Φ(x) = 1 - 8/3 x + 4/3 x2

polyroot(c(1,-8/3,4/3))

### (2) . Which, if any, of the following processes are invertible?

###(a) Solutions of polynomial Θ(x) = 1 - 2 x

polyroot(c(1,-2))

###(b) Solutions of polynomial Θ(x) = 1 - 1/2 x

polyroot(c(1,-1/2))

### (3) Identify each of the following as an ARIMA model by giving the values of (p, d, q)

###(a) 
###Solutions of polynomial Φ(x) = 1 - ⅚ x + ⅙ x2
polyroot(c(1,-5/6,1/6))

### Solutions of polynomial Θ(x) = 1 - ¼ x2 
polyroot(c(1,0,-1/4))

###(b)
###Solutions of polynomial Φ(x) = 1 - ¾  x2 - ¼  x3
polyroot(c(1,0,-3/4,-1/4))

###Solutions of polynomial  Θ(x) = 1 -1 x +  ¼ x
polyroot(c(1,-1,1/4))

--------------------------------------------------------------------------------------------
##Problem 3

## 1
  #Reading the data
euro <- read.table("EURUSD=X.csv",sep=",",dec=".",header=T)
  #Creating time series object
euro.ts <- ts(euro[,5], start =c(2003,12) ,end=c(2022,7), frequency = 7) 
  #Plotting the time series
plot(euro.ts, main = "Historical daily exchange rate of Euro/USD from Dec 2003 to July 2022",
     xlab="Time in weeks from Dec 2003 to July 2022", ylab= "Exchange rate of Euro/USD")

## 2
#Tests for non-stationarity

adf.test(euro.ts)

pp.test(euro.ts)

kpss.test(euro.ts)

#time series object found to be Not stationary

##3
#Fit an ARIMA(1,1,0)-model
eur.arima <- arima(euro.ts, order = c(1,1,0))
eur.arima
AIC(eur.arima)

##4
#Plotting the residuals of the arima model
plot(resid(eur.arima), main="Residual plot of the ARIMA(1,1,0) model")
resid.euro <- resid(eur.arima)

#Plotting the acf and pacf of the residuals
plot(acf(resid(eur.arima)), main="Correlograms of the residuals of the ARIMA model")
plot(pacf(resid.euro), main="Partial correlograms of the residuals of the ARIMA model")

##5
#Tests for white noise
Box.test(resid.euro, type = "Ljung-Box")

Box.test(resid.euro, type = "Box-Pierce")

##6
#Fitting GARCH(1,1) on the arima model
euro.garch <- garch(resid.euro, order = c(1,1), trace = F) 
euro.garch
summary(euro.garch)
AIC(euro.garch)

-----------------------------------------------------------------------------------------------
##Problem 4

##1 Load the required data and create time series object
runoff<- read.table("runoff.txt",header=T)
Weser<- ts(runoff$Calculated, start=c(1857,1), end = c(2016,12), frequency = 12) 

#Plotting the time series object
plot(Weser, main = "Monthly run off data from January 1857 to December 2016 from
     German River Weser",
     xlab="Time in months from Jan 1857 to Dec 2016", ylab= "Runoff data" )

##2 Choosing the best-fitting seasonal ARIMA model from the following models

AIC(arima(Weser, order = c(1,1,0), seas = list(order = c(1,1,0), 12)))
AIC(arima(Weser, order = c(1,1,0), seas = list(order = c(0,1,1), 12)))
AIC(arima(Weser, order = c(0,1,1), seas = list(order = c(1,1,0), 12)))
AIC(arima(Weser, order = c(1,0,0), seas = list(order = c(1,0,1), 12)))
AIC(arima(Weser, order = c(1,1,1), seas = list(order = c(1,1,0), 12)))
AIC(arima(Weser, order = c(1,0,1), seas = list(order = c(0,1,0), 12)))

##3 Best fitted ARIMA model

## Best AIC value
arima.best <- arima(Weser, order = c(1,0,0),
      seas = list(order = c(1,0,1), 12))
arima.best

#Plotting the residuals and correlograms of the best model
plot(acf(resid(arima.best)))

plot(resid(arima.best))

##4 Predict the value for January 2017 

pred = predict(arima.best, n.ahead = 1)
pred
#---------------------------------------------------------------------------------------------
