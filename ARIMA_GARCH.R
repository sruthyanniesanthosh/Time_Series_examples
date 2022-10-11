setwd("D:/Hildesheim Universitaet/Semester 2/Time Series Analysis/Exercises and Solutions/HW 5")

#Q1)
loan <- "mprime.txt"
Loan <- read.table(loan)
loan.ts <- ts(Loan, start = c(1949, 1), end = c(2007, 11), frequency = 12)
plot(loan.ts)

acf(loan.ts)

#b)
loan.ar <- ar(loan.ts)
loan.ar



loan.res <- loan.ar$resid
plot(ts(loan.res))
acf(loan.res, na.action = na.pass)
acf(loan.res^2, na.action = na.pass)

#c)
library(tseries)
library(TSA)
garch11 <- garch(loan.res[18:707],order = c(1,1),trace=F) 
AIC(garch11)

garch(loan.res[18:707],order = c(1,0),trace=F)-> garch10
AIC(garch10)


garch(loan.res[18:707],order = c(0,1),trace=F)-> garch01
AIC(garch01)


#q2)

library(TSA)
#co2<-data(co2)
y=co2
plot(y)
data.ts <- ts(y, start = c(1994, 1), end = c(2004, 12), frequency=12)
plot(data.ts)


acf(data.ts)

#b)
yt <- diff(co2, differences = 1)
plot(yt)
acf(yt)


#c)
zt <- diff(yt, difference=12)
plot(zt)
acf(zt)


#d)

