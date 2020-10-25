

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

#Random Walks are NonStationary processes (varying mean, non bounded variance)
#Random Walk is an autoregressive process: each observation is correlated to the one before

#Simulate 1000 data points at random walk
rw <- ts(cumsum(rnorm(1000)))
plot(rw)
acf(rw, lag.max = 100 , type = "correlation")
#putting the command above fully in between "()" will print out all the coefficients

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

#Moving averages are (weakly) Stationary processes (constant mean, limited variance)

#Simulating MA(2) (order q=2)
#noise:
ma.noise <- rnorm(10000)
#variable placeholder
ma.2 <- c()
#fill ma.2 (starts in position 3, since it will a mocing average of 2 previous periods - MA(2))
for (i in 3:10000) {
        ma.2[i] <- ma.noise[i] + 0.8*ma.noise[i-1] + 0.4*ma.noise[i-2]  #the coefficients here are arbitrary
}                                                                       #could be anything (<1)
#shift data to the left by two units and put time-series structure
ma_process <- ts(ma.2[3:1000])

#plot and correlogram-it-out
plot(ma_process)
acf(ma_process, type = "correlation")
#note from the correlogram that the r dies out at the degree of the MA
#for an MA, autocorrelation coefficients will be significant until the point of the MA degree/order; at q=order+1, r=0


#A moving average process of order q, MA(q), has an ACF that cuts off after q lags

#So, if you have an MA() process with enough terms that you believe the ACF is well estimated,
#and the ACF cuts off after 4 lags, you can be reasonably sure you have an MA(4) process.

## AUTOREGRESSIVE PROCESSES

#Simulation 1, AR(1)
z <- rnorm(1000)
phi <- 0.4
x <- NULL
x[1] <- z[1]

for (i in 2:1000) {
        x[i] <- z[i] + phi*x[i-1]
}
x.ts <- ts(x)
plot(x.ts)
acf(x , type = "correlation")


#Simulation 2 (arima.sim() from the "stats" package) AR(2)
x.ts2 <- arima.sim(list(ar=c(0.5,-0.4)), n=1000)   #we are asking for coefficients 0.2 (for xt-1) and 0.7 (for xt-2)
                                                   #for an autoregression ("ar") (it could be "ma" for moving average)
plot(x.ts2)
acf(x.ts2 , type = "correlation")


#The PACF - Partial Autocorrelation Function
#The PACF help us find the order of an AR(p) process

#Simulation 1
phi.1 = .6
phi.2 = .2

data.ts = arima.sim(n = 500, list(ar = c(phi.1, phi.2)))

plot(data.ts, main=
             paste("Sim.1 | Autoregressive Process with phi1=",phi.1," phi2=",phi.2) )

acf(data.ts, main="Autocorrelation Function")
acf(data.ts, type="partial", main="Partial Autocorrelation Function")


#Simulation 2
phi.1 = .9
phi.2 = -.6

data.ts = arima.sim(n = 500, list(ar = c(phi.1, phi.2)))

plot(data.ts, main=
             paste("Sim.2 | Autoregressive Process with phi1=",phi.1," phi2=",phi.2) )

acf(data.ts, main="Autocorrelation Function")
acf(data.ts, type="partial", main="Partial Autocorrelation Function")


#Simulation 3
phi.1 = .9
phi.2 = -.6
phi.3 = .3

data.ts = arima.sim(n = 500, list(ar = c(phi.1, phi.2, phi.3)))

plot(data.ts, main=
             paste("Sim.3 | Autoregressive Process with phi1=",phi.1," phi2=",phi.2," phi3=",phi.3) )

acf(data.ts, main="Autocorrelation Function")
acf(data.ts, type="partial", main="Partial Autocorrelation Function")

#An autoregressive process of order p, an AR(p), has a PACF that cuts off after p lags.

#So if we know that we have an autoregressive process and are looking to determine the order of the
#process, we produce a PACF and observe where it “cuts off

#In the PACF plot, we remove the linear effects of all the terms between the two random variables (i.e the lags
#between lags).
#The excess correlation at lag=k not accounted for by a (k-1)st order model, is the partial correlation at lag K.




#Yule-Walker equations in matrix form to estimate the parameters of fitted model
#An example below

library(astsa)                  #J&J quarterly earnings dataset

# Time plot for Johnson&Johnson
plot(JohnsonJohnson, main='Johnson&Johnosn earnings per share', col='blue', lwd=3)

#The data shows systematic variation, with trend and seasonality, so not Stationary -> we cannot fit an AR model here yet
#We need to transform this time-series and make it stationary in order to analyze it

#We will perform a transformation of "log-return", which means taking the differences of the log(time-series)
#i.e logging it up, and then differencing it out

# log-return of Johnson&Johnson
# First: differencing the log of the time-series
jj.log.return=diff(log(JohnsonJohnson))
#Then: make it relative to the mean (so, the new ts will be stationary around mean zero)
#This is required for Yule-Walker equations
jj.log.return.mean.zero=jj.log.return-mean(jj.log.return)

# Plots for log-returns
par(mfrow=c(3,1)) #Partition the screen in 3 rows, 1 column
plot(jj.log.return.mean.zero, main='Log-return (mean zero) of Johnson&Johnosn earnings per share')
acf(jj.log.return.mean.zero, main='ACF')
pacf(jj.log.return.mean.zero, main='PACF')

# Order: from the PACF chart we see that significancy dies out after lag 4 (in quarters)
p=4

# sample autocorreleation function r
r=NULL
r[1:p]=acf(jj.log.return.mean.zero, plot=F)$acf[2:(p+1)]
r

# matrix R
R=matrix(1,p,p) # matrix of dimension 4 by 4, with entries all 1's.

# define non-diagonal entires of R
for(i in 1:p){
        for(j in 1:p){
                if(i!=j)
                        R[i,j]=r[abs(i-j)]
        }
}
R

# b-column vector on the right
b=matrix(r,p,1)# b- column vector with no entries
b

phi.hat=solve(R,b)[,1]
phi.hat

# Variance estimation using Yule-Walker Estimator
c0=acf(jj.log.return.mean.zero, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat

# Constant term in the model
phi0.hat=mean(jj.log.return)*(1-sum(phi.hat))
phi0.hat

cat("Constant:", phi0.hat," Coefficients:", phi.hat, " and Variance:", var.hat, '\n')



## MODEL SELECTOIN CRITERIAS

#Simulation 1: generate and AR(2) process

#Specifying parameters:
# order = c(2,0,0)
# ar =c( 0.7, -.2)

#the vector "order" is listed as (p,d,q), so we are generating and AR(p=2)
#and its coefficients are listed in the vector "ar"

data = arima.sim(list(order = c(2,0,0), ar =c( 0.7, -.2)), n = 10000)
par(mfrow=c(1,2))
acf(data, main="ACF of AR Data of Second Order")
acf(data, type="partial", main="PACF of Time Series")

#Estimate the parameters of the model with the function arima(),
#but we need to tell it the order of the process

arima(data, order=c(2,0,0))
#Observe that the estimated parameter are, indeed, what we set them to be.

#However, we will likely never know the order opf the process beforehand
#So let's try and generate estimates for various values of p:

SSE=NULL
AIC=NULL
for (p in 1:5) {
        m = arima(data, order=c(p,0,0))
        SSE[p] = sum(resid(m)^2)
        AIC[p] = m$aic
        print( m$coef )
        print( paste(m$aic, sum(resid(m)^2)) )
}

#From the output we observe that there's a big drop in both AIC and SSE from
#a first order model (p=1) to a second order model (p=2)
#Then, from p=2 to p=3, AIC and SSE do not change significantly and the
#added coefficient (ar3) is nearly zero
#Finally, from p=3 to p=4, AIC and SSE start both rising again
#We may conclude, therefore, that this model has p=2

#(similarly to the adj-R^2, the AIC sets a penalty to increasing number of variables in the model)
#so the best model is the one with the lowest AIC and/or SSE


## MIXED MODELS : AR(p) + MA(q) : ARMA(p,q)

# For an ARMA(p,q) model, we will have:

                                #xt = noise + AR piece + MA piece

#Real life time-series have many components (trend, seasonality, noise) and are most likely always NonStationary
#More often than not, a mixed model will be more efiicient in provinding us with less coefficients to describe it
#i.e. modeling it via ARMA(p,q), rather than an AR(p) or MA(q)


#Simulaion of ARMA(p,q)

data = arima.sim(list(order = c(1,0,1), ar=0.7 , ma=0.2), n = 10000)

par(mfcol = c(3,1))
plot(data)
acf(data, type = "correlation")
acf(data, type = "partial")


## MIXED MODELS: ARIMA(p,d,q)

#A note on differencing: we should stick to 1<=d<=2
#Over-differencing may introduce dependence
#A very slowly decaying ACF is indication that the time-series may require differencing

#Simulaion of ARIMA(p,d,q)

data = arima.sim(list(order = c(2,1,1), ar=0.7 , ma=0.2), n = 10000)

par(mfcol = c(3,1))
plot(data)
acf(data, type = "correlation")
acf(data, type = "partial")



## A COMPLETE EXAMPLE - ARIMA FITTING

library(astsa)

# read data to R variable
birth.data<-read.csv("daily-total-female-births-in-cal.csv")

# pull out number of births column
number_of_births<-birth.data$Daily.total.female.births.in.California..1959

# use date format for dates
birth.data$Date <- as.Date(birth.data$Date, "%m/%d/%Y")

plot.ts(number_of_births,main='Daily total female births in california, 1959', ylab = 'Number of births')
#We observe some trend in the data; NonStationary

# Test for correlation
Box.test(number_of_births, lag = log(length(number_of_births)))
#p-value significantly low: there is some autocorrelation going on

# Plot the differenced data
plot.ts(diff(number_of_births), main='Differenced series', ylab = '')

# Test for correlation in the differenced data
Box.test(diff(number_of_births), lag = log(length(diff(number_of_births))))
#p-value significantly low: there is some autocorrelation going on

# acf and pacf of the differenced data
acf(diff(number_of_births), main='ACF of differenced data', 50)
pacf(diff(number_of_births), main='PACF of differenced data', 50)
#ACF is consistent with an MA(1) model, though we can't be sure yet
#PACF is consistent with an AR(7) mode, though we can't be sure yet

# Fit various ARIMA models

#we will have d=1 in all of them, because we had to detrend i.e. perform differencing to analyze the data
#we will also perform box-test in each of the models to see whether residuals have autocorrelation (they should not
#have autocorrelation in the final chosen model - we want residual to be whiute noise only)

model1<-arima(number_of_births, order=c(0,1,1))
SSE1<-sum(model1$residuals^2)
model1.test<-Box.test(model1$residuals, lag = log(length(model1$residuals)))

model2<-arima(number_of_births, order=c(0,1,2))
SSE2<-sum(model2$residuals^2)
model2.test<-Box.test(model2$residuals, lag = log(length(model2$residuals)))

model3<-arima(number_of_births, order=c(7,1,1))
SSE3<-sum(model3$residuals^2)
model3.test<-Box.test(model3$residuals, lag = log(length(model3$residuals)))

model4<-arima(number_of_births, order=c(7,1,2))
SSE4<-sum(model4$residuals^2)
model4.test<-Box.test(model4$residuals, lag = log(length(model4$residuals)))

df<-data.frame(row.names=c('AIC', 'SSE', 'p-value'), c(model1$aic, SSE1, model1.test$p.value), 
               c(model2$aic, SSE2, model2.test$p.value), c(model3$aic, SSE3, model3.test$p.value),
               c(model4$aic, SSE4, model4.test$p.value))
colnames(df)<-c('Arima(0,1,1)','Arima(0,1,2)', 'Arima(7,1,1)', 'Arima(7,1,2)')

format(df, scientific=FALSE)

#We observe that all p-values are quite large: we don not have enough evidence to reject the null hypothesis that
#there is no autocorrelation between them
#from this stand point, all models are a good fit, leaving only white noise behind
#looking at AIC, arima(0,1,2) seems to be the best fit
#with SSE, however, arima(7,1,2) seems like the best fit, but has many more coefficients
#choosing the most parsimonious model, we will pick arima(0,1,2)
#Furthermore, the p-values shown are for autocorrelation test on residuals - so we want high p-values, meaning that
#there is no aitocorrelationleft in the residuals: they are white noise only!


## FROM COURSE QUIZ - BJSales

data <- data(BJsales)
plot(BJsales)

#Trend is present

data.diff <- plot(diff(BJsales))
#Still not stationary even after detrending: it must require a complex model arima(p,d,q) with d!=0

data.diff2 <- plot(diff(diff(BJsales)))
#Trying to re-detrend the difference still produces different variance at the end of the plot (more recent times)

acf(diff(diff(BJsales)), type = "partial")
#The plot is consistent with AR(3), not considering the later correlations at lags 10 and 19

acf(diff(diff(BJsales)), type = "correlation")
#The plot is consistent with MA(1), not considergint the later barely significant correlations at lags 8 and 11

#Let us try a few different models and compare their AIC and SSE values
#We have applied differencing twice, so we will set d=2
#We have seen that it is likely MA(upto1) and/or AR(upto4)

d<-2

for (p in 1:4) {
        for (q in 1:2) {
                if(p+d+q<=6){       #arbitrary from Parsimony Principle: we don't want more than 6 coefficients
                        model <- arima(BJsales, order = c((p-1),d,(q-1)))
                        pval <- Box.test(model$residuals, lag = log(length(model$residuals)))
                        sse <- sum(model$residuals^2)
                        cat(p-1 , d , q-1, "AIC=" , model$aic , "SSE=" , sse , "p-value=" , pval$p.value , "\n")
                }
        }
}

#From the output,
#arima(0,2,1) has the smallest AIC value
#arima(1,2,1) has the smallest SSE value

#Let us fit the arima(0,2,1 model) and look at the resaiduals plots

model <- arima(BJsales, c(0,2,1))

par(mfrow = c(2,2))

plot(model$residuals)
acf(model$residuals, type = "correlation")
acf(model$residuals, type = "partial")
qqnorm(model$residuals)

#We observe that residual from arima(0,2,1)
#seem stationary, with no particular trends or seasonality
#dont have autocorrelation (ACF and PACF)
#are normally distributed around zero

#An arima(0,2,1) seems to fit this time-series well

#Now that we have picked our time-series model for this dataset, let's try and forecast it:

library(forecast)
data.fcst <- forecast(model, h=24 , level = 0.80)
autoplot(data.fcst)

#What do other non picked models look like when used to forecast BJsales:

model.1 <- arima(BJsales, c(0,2,0))
model.2 <- arima(BJsales, c(1,2,0))
model.3 <- arima(BJsales, c(1,2,1))
model.4 <- arima(BJsales, c(2,2,0))

fcst.model.1 <- forecast(model.1, h=24 , level = 0.80)
fcst.model.2 <- forecast(model.2, h=24 , level = 0.80)
fcst.model.3 <- forecast(model.3, h=24 , level = 0.80)
fcst.model.4 <- forecast(model.4, h=24 , level = 0.80)

g0 <- autoplot(data.fcst)
g1 <- autoplot(fcst.model.1)
g2 <- autoplot(fcst.model.2)
g3 <- autoplot(fcst.model.3)
g4 <- autoplot(fcst.model.4)

library(gridExtra)

grid.arrange(g0,g1,g2,g3,g4, nrow=2,ncol=3)


## DEALING WITH SEASONALITY - SARIMA

#SARIMA Simulation

x=NULL
z=NULL
n=10000

z=rnorm(n)
x=rep(1,13)

for(i in 14:n){
        x[i]<-z[i]+0.7*z[i-1]+0.6*z[i-12]+0.42*z[i-13]
}

par(mfrow=c(2,1))
plot.ts(x[12:120], main='The first 10 months of simulation SARIMA(0,0,1,0,0)_12', ylab='') 

acf(x, main='SARIMA(0,0,1,0,0,1)_12 Simulation')
#Observe that this ACF shows a spike at lag = 1 : this is by design, since we have an MA(1) process inside our model
#(given by the 0.7*z[i-1] piece of the equation, meaning that every Xt-1 has an influence in every Xt)
#Then, we observe spikes at lag 11, 12 and 13 : there is seasonality in our data (also, by design) : the same month
#from the previous year has an influence in that month in the current year
#But wy do we have a spike at lag 11 (why do we have autocorrelation at lag 11), since our equation does not
#include a z[i-11] component?
#Because there is correlation betwee Xt and Xt-1 and Xt-12 ; so from the covariance formulas, Xt-12 will be a common
# appearance across Xt and Xt-11!! AS a result: autocorrelation between Xt and Xt-11 as well!

# EXAMPLE 1 | SARIMA

library(astsa)

d=1
DD=1

per=4

for(p in 1:2){
        for(q in 1:2){
                for(i in 1:2){
                        for(j in 1:2){
                                if(p+d+q+i+DD+j<=10){
                                        model<-arima(x=log(jj), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
                                        pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
                                        sse<-sum(model$residuals^2)
                                        cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
                                }
                        }
                }
        }
}

#From the output, we see that the best fitted model is sarima(0,1,1,1,1,0)_4 (lowest AIC, parsimonial)
#Since the dataset has quarterly figures, it was expected for the seasonality to be =4 (and we have set that way in
#the simulation)

#sarima() function from the "astsa" package
sarima(log(jj), 0,1,1,1,1,0,4)


#Forecasting with the chosen model
model <- arima(
                log(jj) ,                                           #data
                order = c(0,1,1),                                   #ARIMA piece
                seasonal = list(order = c(1,1,0), period = 4))      #S piece

#Notice thaty you can use the arima() with a "seasonal" argument, or sarima(), to fit a SARIMA model.

library(forecast)
plot(forecast(model))
forecast(model)



# EXAMPLE 2 | SARIMA | Sales at Souvenir Shop

suv <- ts(read.csv("sales_souvenir.csv"))

library(astsa)
library(forecast)

par(mfrow=c(2,2))

plot(suv, main='Monthly sales for a souvenir shop', ylab='', col='blue', lwd=3)
plot(log(suv), main='Log-transorm of sales', ylab='', col='red', lwd=3)
plot(diff(log(suv)), main='Differenced Log-transorm of sales', ylab='', col='brown', lwd=3)
plot(diff(diff(log(suv)),12), main='Log-transorm without trend and seasonaliy', ylab='', col='green', lwd=3)

#Chart 1, blue: we see trend and seasonality in sales, variance is increasing
#Chart 2, red: log-transforming it adjusted for some exponential growth; we have stabilized the variation
#Chart 3, brown: apply first differencing to detrend it. Seasonality remains.
#Chart 4, green: apply second differncing to get rid of the seasonality, of order=12 (annual seasonality, monthly data)
data<-diff(diff((log(suv)),12))

#function acf2() already produces automatically the ACF and PACF plots!
acf2(data, 50)
#ACF chart suggests an MA(1) process -> so we'll probably have q=1
#Additionally, we have barely significant points at lags 20something and 30ish, suggesting a Q component
#PACF chart suggests an AR(1) process
#There are no additional later lags, suggesting no P component


#So let's try and fit a few models into this time-series
#we will establish our parsimony principle

d=1       #we have done one level of differencing for detrending the log transformed data, so d=1
DD=1      #then, we have also performed one additional differencing for the seasonality component, so D=1
per=12
for(p in 1:2){
        for(q in 1:2){
                for(i in 1:2){
                        for(j in 1:4){
                                if(p+d+q+i+DD+j<=10){
                                        model<-arima(x=log(suv), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
                                        pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
                                        sse<-sum(model$residuals^2)
                                        cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
                                }
                        }
                }
        }
}

#From the output, the best fitted model (parsimony + low AIC + low SSE) is:
#SARIMA(1,1,0,0,1,1)_12

#As we suspected:
#there is a d component = 1 (by design)
#a D component = 1 (by design)
#a p component = 1
#and a Q component = 1

model<- arima(x=log(suv), order = c(1,1,0), seasonal = list(order=c(0,1,1), period=12))
autoplot(forecast(model))
forecast(model)

#We will take a look at the residual analysis
sarima(log(suv), 1,1,0,0,1,1,12 )
#We see no autocorrelation amongst residuals, which are normally distributed around zero, with a few extreme values, though.


#The function sarima.for() produces arima forecasting with arguments:
#sarima.for(data , forecast horizon, p,d,q,P,D,Q , S=Seasonal Period)
#So, let's generate the forecast of our log-transformed data, using the parameters of our chosen model,
#for the next 12 months:
a<-sarima.for(log(suv),12,1,1,0,0,1,1,12)

#The function plot.ts() has built-in specific paramenters for time-series plotting
#We are interested in the final forecasted sales, not the log-transformed version
#So we are plotting the original data "suv" with the predicted sales "exp(a$pred)"
plot.ts(c(suv,exp(a$pred)), main='Monthly sales + Forecast', ylab='', col='blue', lwd=3)



## FORECASTING WITH SIMPLE EXPONENTIAL SMOOTHING (SES)

rain.data <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rain.ts <- ts(rain.data, start=c(1813))

par( mfrow=c(1,2) )
hist(rain.data, main="Annual London Rainfall 1813-1912",
     xlab="rainfall in inches")
qqnorm(rain.data,main="Normal Plot of London Rainfall")
qqline(rain.data)


plot.ts(rain.ts, main="Annual London Rainfall 1813-1912",
        xlab="year", ylab="rainfall in inches")
acf(rain.ts, type = "correlation", main="ACF: London Rainfall")
acf(rain.ts, type = "partial", main="PACF: London Rainfall")

#Our ACF charts are not very helpful right now: we don't see any significant correlations appear
#Even auto.arima() kind of gives up on us

library(forecast)
auto.arima(rain.ts)

#It gives us the model ARIMA(0,0,0) and says we should move forward with the average alone.

#We can move to the Holt-Winters methos (see pdf "Introduction to Forecast_SSE" in the folder for details)

HoltWinters(rain.ts, beta=FALSE, gamma=FALSE)
#Here, we called a Holt-Winters without trend and without seasonal component
#THe alpha given by this method is the one that minimizes SSE.

#The component alpha will tell us how far away in the past we are looking to consider the forecasts for future values
#The larger the alpha (closer to 1), the heavier the weight for more recent values;
#For alpha =1, X(t+1) = X(t); for alpha=1, we would be predicting that the next value in the time-series will just be
#the same as thje last value.
#The smaller the alpha, the further in the past we are considering to predict future values.

#Conclusion: we can build a forecast by taking a weighted average of past values,
#with weights given corresponding to the geometric series (i.e. exponentially decreasing).
#This gives a forecasting method called Simple Exponential Smoothing or SES.
#This method gives greater weight to those values in the series closer to the forecast, and
#lesser weight to terms further in the past.

#While SES this is not a bad approach, it is certainly limited and doesn’t include some other factors
#which may be driving your system. We would like to build on this approach but also include
#seasonal and trend effects.

#The Holt-Winters method with alpha, beta and gamma, also called triple exponential smoothing, does just that.
#alpha: levels
#beta: trend
#gamma: seasonality

#Pros of this method:
#low data storage requirements
#easily automated
#adapts to changes in trends and seasonal patterns: slow-downs or speed-ups in demand, or changes in consumer beahviour
#may all be accomodated as new data comes in.


## Let us see another example

data <- AirPassengers
plot(data)

#ARIMA Approach

#We observe upwards trend and a seasonality, as well as increasing variance as we move to more recent times

#Remember: log-transforming the data should rid us the increasing variance
data.log <- log(data)
plot(data.log)

#Still not stationary;
#We may differece it to detrend

data.log.diff1 <- diff(log(data))
plot(data.log.diff1)

#Seems stationary now.

#Then we may analyze the autocorrelations:
library(astsa)
acf2(data.log.diff1)
#ACF suggests an MA process with q=1 and Q=1
#PACF suggest an AR process with p=1 and P=1
#And seasonality S=12

#Let us see what the calculation tell us
auto.arima(data.log)

#The suggest model is SARIMA(0,1,1,0,1,1)_12
model<- arima(data.log, order = c(0,1,1), seasonal = list(order=c(0,1,1), period=12))
autoplot(forecast(model))
forecast(model)

#Let us analyze the residuals:
sarima(data, 0,1,1,0,1,1,12)

#We may build the forecast and plot it directly at one go with the function sarima.for() from "astsa" package
fcst <- sarima.for(data.log,24,0,1,1,0,1,1,12)

#We are interested in the forecast in number of passangers, not log:
plot.ts(c(data,exp(fcst$pred)), main='Monthly sales + Forecast', ylab='Passengers', lwd=3)

#THe forecasted number of passengers may retrieved from:
fcst$pred #log
exp(fcst$pred) #Number of passengers


#HOLT-WINTERS APPROACH

library(forecast)
library(astsa)
data <- AirPassengers
data.hw <- HoltWinters(data)
data.hw.fcst <- forecast(data.hw)
#The function forecast() already recognizes that this is a Holt-Winters model, becaude it was built with HoltWinters()
autoplot(data.hw.fcst)

#THe forecasted number of passengers may retrieved from:
data.hw.fcst$mean #Number of passengers

#WE MAY COMPARE THE PREDICTIONS FROM BOTH MODELS
compare <- data.frame("month" = c(1:24) , "with SARIMA" = exp(fcst$pred) , "with HW" = data.hw.fcst$mean)

library(ggplot2)

ggplot(compare) +
        geom_line(aes(month, with.SARIMA, colour = "blue")) +
        geom_line(aes(month, with.HW, colour = "red")) +
        labs(x = "Month" , y = "Number of passengers" , colour = "Method: blue = SARIMA | red = HW")




















