##Homework Assignment 3 - Sruthy Annie Santhosh, 312213
##set the directory and importing the required libraries

setwd("/Users/sruthysanthosh/Desktop/Sem2")

library("timeSeries")

## (1) Exercise 1

##a) Xt = 2.25Xt−1 − 0.5Xt−2 + Zt 

## Finding the root of the polynomial function: Φ(x) = 1 - 2.25 x + 0.5 x2 

polyroot(c(1, -2.25, 0.5))

##b) Xt = Xt−1 − 0.21Xt−2 + Zt .	 

## Finding the root of the polynomial function: Φ(x) = 1 -  x + 0.21 x2 

polyroot(c(1, -1, 0.21))


## (2) Exercise 2

##a)

set.seed(10)

x<- z<- rnorm(1000,mean = 0, sd = 1)
for (t in (3:1000)) x[t]<- 0.4*x[t-1]-0.3*x[t-2] +z[t]

x

##b)
acf(x, lag.max=30)
pacf(x, lag.max=30)

## In contrast to the ACF, we see from the PACF that almost 
##only the first two lags are significant, which indicates 
## that the underlying process could be AR(2).


##c)
x.ar<- ar(x, method = "mle")
x.ar

x.ar[1]

### So we get a fitted AR Model with Xt=0.4464*Xt-1- 0.3136*xt-2 +Zt 
##each with mean 0 and variance 0.9804


##d)
x.ar.residual <- x.ar$resid[-(1:x.ar$order)]
acf(x.ar.residual, lag.max = 30)
plot(x.ar.residual,xlab="Each index in the x series",ylab="Residual values",
     main="Residuals after fitting AR(2) to the X series")

#Box test for IID

#for lag = 1 to 10
for(i in 1:10){ print(Box.test(x.ar.residual, lag = i, type = "Box-Pierce"))}  

#Turning point test for IID
library(randtests)

turning.point.test(x.ar.residual)

#Shapiro test for Gaussianity
shapiro.test(x.ar.residual)  



#Dnorm 
plot(density(x.ar.residual), main="Density of X series fitted with AR(2) residuals")
mean <-mean(x.ar.residual)
std <- sd(x.ar.residual)
curve(dnorm(x,mean,std),c(-8,6),add=T,col="red")




