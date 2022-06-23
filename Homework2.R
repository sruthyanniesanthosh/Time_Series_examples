##Homework Assignment 2 - Sruthy Annie Santhosh, 312213
##set the directory and importing the required libraries

setwd("/Users/sruthysanthosh/Desktop/Sem2")
library("readxl")
library("timeSeries")

## (1) Import the data into R and then plot it.
temperature <- read.table("global.dat", header = TRUE)

temp.ts <- ts(scan("global.dat"), start = c(1856,1), 
              end = c(2005,12), frequency = 12)

plot(temp.ts, main = "Global temperature series 
     expressed as anomalies from the monthly means over the time 
     period 1856-2005",
     xlab="Time in months from 1856 to 2005", ylab= "Temperature anomalies ")

## (2) Use the aggregate function to remove any seasonal effects within each
##      year and plot an annual series (called {xt}) of mean temperatures for
##      the period 1856-2005.

temp.annual <- aggregate(temp.ts)/12

plot(temp.annual, xlab = "Time in years from 1856 to 2005", ylab = "Temperature anomalies",
     main = "Annual Temperatures from 1856 to 2005")

## (3) Determining the coefficients using least square method

TIME <- time(temp.annual)
#Using linear regression model
temp.lm <- lm(temp.annual~TIME)
#Finding the coefficients alpha and beta
coef(temp.lm)

##(4) Use the command abline to draw a regression line to the existing plot

abline(temp.lm)

summary(temp.lm)

##(5) Plot the random series {rt}.

plot(resid(temp.lm), main="Plotting the residuals of the regression",
     xlab="Each Year from 1856 to 2005")

x<-decompose(temp.ts,type="multiplicative")
y = x$random

plot(x$random, main="Random series {rt}",xlab="Time in years from 1856 to 2005",
     ylab="Rt values")
resid(temp.lm)

##(6) Carry out the turning point*, Box-Pierce, Ljung-Box and difference
### sign* tests for the random series {rt}. 
### In the box tests, calculate all p-values for lag = 1, 2, . . . , 24.
library(randtests)


turning.point.test(resid(temp.lm))

#for lag = 1 to 24
for(i in 1:24){ print(Box.test(resid(temp.lm), lag = i, type = "Box-Pierce"))}  
## print-command needed to ensure printing

for(i in 1:24){ print(Box.test(resid(temp.lm), lag = i, type = "Ljung-Box"))}  


difference.sign.test(resid(temp.lm))
difference.sign.test(temp.annual)

