
##Homework Assignment 1 - Sruthy Annie Santhosh, 312213
##set the directory and importing the required libraries

setwd("/Users/sruthysanthosh/Desktop/Sem2")
library("readxl")
library("timeSeries")

## (a) Import the data into R and then plot it.
data.price <- read_excel("statistic-mobile-communications.xlsx",
                        sheet="Data")
attach(data.price)

series <- ts(data.price$CPI[1:168],frequency=1)
series.yearly.ts <- ts(data.price$CPI,start=c(2005,1), end=c(2018,12), frequency=12)

plot(series, ylab = "Consumer Price Index Baseline", xlab="Time in months from Jan 05 to Dec 18 ",
     main = "Monthly consumer price index (CPI) for mobile communications 
     services in Germany from January 2005 to December 2018.",
     type = "l",xaxt="n")
axis(1,at=seq(168),labels=Time )

##(b) Decompose the time series into three parts: estimating trends, 
##    seasonal effects, and random series.
plot(decompose(series.yearly.ts))


##(c) Use the aggregate function to remove any seasonal effects within each year
##    and produce an annual series of mean CPI for the period 2005- 2018.
series.yearly <- aggregate(series.yearly.ts) / 12

plot(series.yearly, ylab="CPI",main="Yearly CPI for mobile communications in 
     Germany from Jan 2005 to Dec 2018", xlab="Time in years", type='l')


##(d) Use the window function to plot the data from January 2005 to December 2017.
price.win <- window(series.yearly.ts, end=c(2017,12) )
plot(price.win, ylab = "Consumer Price Index Baseline", xlab="Time in years",
     main = "Monthly consumer price index (CPI) for mobile communications 
     services in Germany from January 2005 to December 2017.")


##(e) Use the command lm to estimate the parameters αˆ and βˆ in the simple
##    linear regression model.
TIME.price <- time(price.win)
lm.price <- lm(price.win~TIME.price)
lm.price

##(f) Use commands summary and abline to add lines to existing plots in step (e).
summary(lm.price)
abline(lm.price)


##(g) Make predictions "by hand" for June 2018, June 2019 and June 2020.

## 5694 +-2.78218 * (2018+ 5/12) = 5694 - 5615.59 = 78.401 --- June 2018
## 5694 +-2.78218 * (2019+ 5/12) = 5694 - 5618.38 = 75.62 --- June 2019
## 5694 +-2.78218 * (2020+ 5/12) = 5694 - 5621.16 = 72.84 --- June 2020


##(h) Use the Holt-Winters method to get the smoothing parameters α,β and γ for
##    the monthly CPI time series from January 2005 to December 2017.
series.hw <- HoltWinters(price.win)

plot(series.hw, ylab="CPI values", xlab="Time in years")

series.hw$alpha
series.hw$beta
series.hw$gamma

##(i) Use the HoltWinters function to plot the time series at, bt and st 
##    introduced in step (h).
### Plot the HW decomposition
plot(series.hw$fitted, main="HW decomposition", xlab="Time in years")


##(j) Make predictions "by the Holt-Winters method" for each month in 2018, 2019
##    and 2020.
series.hw.predict <- predict(series.hw, n.ahead = 3*12)
series.hw.predict
plot(series.hw.predict, main="Prediction of CPI values from Jan 2018 to Dec 2020",
     ylab="CPI values")


plot(series.hw, main="Monthly consumer price index (CPI) for mobile communications 
     services in Germany from January 2005 to December 2020",ylim=c(70,150),
     xlim=c(2004,2021))

lines(series.hw.predict, col="blue")


