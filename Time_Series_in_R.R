
#Load required packages
library("IRdisplay")
library("magrittr")
library("tidyverse")
library("scales")
library("gridExtra")
library("forecast")
library("tseries")
library("ggthemes")

#Load helper functions
source("compare_models_function.R")
source("sim_random_walk_function.R")
source("sim_stationary_example_function.R")

#Compare two forecast models, linear regression and auto-regression, with n data points
compare.models(n=100)

#simulate random walk
dat<-sim.random.walk()

#plot random walk
dat %>% ggplot(aes(t,X)) + geom_line() + xlab("T") + ylab("X") + ggtitle("Time Series Plot")


#Rules for using ACF and PACF:
##Data cannot present clear trend (it has to be de-trended? look up stationary data assumption and differencing)

#ACF plot (ggAcf function is from the "forecast" package)
#In this plot, vertical lines beyond the horizontal blue lines indicate lags of y that are significant
#predictors of y
ggAcf(dat$X,type="correlation") + ggtitle("Autocorrelation ACF Plot")

#PACF plot
#This plot will show that only lag=1 is a significant predictor of y
ggAcf(dat$X,type="partial") + ggtitle("Partial Autocorrelation PACF Plot")



##CHECKING FOR STATIONARITY

#create three time series for example
df<-sim.stationary.example(n=1000)
head(df)
dim(df)

#X1 and X2 are non-stationary time-series
#X3 is stationary

##Option 1 to check for stationarity: shape of the time-series
#Check a plot of the time series over time and look for constant mean and finite variance (i.e. values appear bounded)
g1 <- ggplot(df, aes(t, X1)) + geom_line() + xlab("t") + ylab("X1") + ggtitle("NonStationary")
g2 <- ggplot(df, aes(t, X3)) + geom_line() + xlab("t") + ylab("X3") + ggtitle("Stationary")
grid.arrange(g1,g2)
# Observe in the first plot:
#NonStationary: the mean doesn't look constant over time, there are apparently no clear boundaries
#Stationary: numbers seem to vary around the same constante mean, without ever getting too far away from it

##Option 2 to check for stationarity: shape of the ACF plot
#ACF plot of the simulated time series above
g1 <- ggAcf(df$X1 , type = "correlation", lag.max = 100) + xlab("t") + ylab("X1") + ggtitle("NonStationary")
g2 <- ggAcf(df$X3 , type = "correlation", lag.max = 100) + xlab("t") + ylab("X3") + ggtitle("Stationary")
grid.arrange(g1,g2)
#NonStationary: gradual decline in correlation as lag increases
#Stationary: correlation approaches zero very quickly


##Option 3 to check for stationarity: unit root hypothesis test such as the Augmented Dickey-Fuller test, ADF test,
##where 
##H0 : time-series is NonStationary and Ha: time-series is Stationary
#significant p-value will mean that we have evidence that the current time-series is Stationary

#perform unit test; nonstationary example has large, non-significant p-value
#with function adf.test() from the "tseries" package
adf.test(df$X1)
#Output is high p-value: we don't have evidence that X1 is Stationary: cannot reject the null hypothesis that it is
#in fact, NonStationary
adf.test(df$X3)
#Output is low p-value: we have evidence that X3 is Stationary





