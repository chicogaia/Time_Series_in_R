

#RECAP ON HYPOTHESIS TEST
plot(extra ~ group, data = sleep)
attach(sleep)
extra.1 <- extra[group==1]
extra.1 <- extra[group==2]

t.test(extra.1, extra.2, paired = TRUE, alternative = "two.sided")

#ggpairs with baseR : pairs(df) -> build charts without cor calculation

##STATIONARITY
#Property we want in a time series for forecasting (although we've seen in course 1 that handy "forecast" package 
#functions will handle NonStationarity for us)
#NO Systematic change in mean. i.e. no trend
#No systematic change in variance
#No periodic variations

#For NonStationary time series we'll do transformarions to make it so

##AUTOCOVARIANCE
#Measures the linear dependence between two variables
#Autocovariance, therefor, measures the dependece between two different observations in  the same variable
#(eg. two different observations separated in time)

#Generate Random time-series data and plot it
purely_random_process <- ts(rnorm(100))
plot(purely_random_process)

#Plot the autocovariance coefficients
acf(purely_random_process, type = "covariance")

#Plot the autocorrelation function (correlogram)
acf(purely_random_process, type = "correlation")
#Notice that lag=0 has always cor=1, beacause it is measuring the correlation of the observations with themselves

##RANDOM WALK MODEL

#Simulate 1000 data points at random walk
rw <- ts(cumsum(rnorm(1000)))
plot(rw)
acf(rw, lag.max = 100 , type = "correlation")

#Another way to simulate 1000 data points at random walk, fixing the starting point at 0
rw <- c()
rw[1] <- 0
for (i in 2:1000) {
        rw[i] <- rw[i-1]+rnorm(1)
}
rw <- ts(rw)
plot(rw)
acf(rw, lag.max = 100 , type = "correlation")

#Removing the trend
#In a random walk, the datapoints are autocorrelated, as seen the ACF above
#However, the differences between them are not (by design, since we generated them with rnorm())
#So, if we take only the differences between points at a given lag (coeff. d), we have now a Stationary time series,
#because the differences are not autocorrelated (as shown by the acf plot below)

rw.diff <- diff(rw) #lag = 1
plot(rw.diff)
acf(rw.diff, type = "correlation")


##MOVING AVERAGE MODEL

#Simulating MA(2) (order q=2)
#noise:
ma.noise <- rnorm(10000)
#variable placeholder
ma.2 <- c()
#fill ma.2 (starts in position 3, since it will a mocing average of 2 previous periods - MA(2))
for (i in 3:10000) {
        ma.2[i] <- ma.noise[i] + mean(c(ma.noise[i-1] , ma.noise[i-2]))
}
#shift data to the left by two units and put time-series structure
ma_process <- ts(ma.2[3:1000])

#plot and correlogram-it-out
plot(ma_process)
acf(ma_process, type = "correlation")





