
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



### CHECKING FOR STATIONARITY

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


### TRANSFORMING FOR STATIONARITY

#Tipically, if we have a NonStationary time-series, we try and transform the data to make it Stationary by 
##Differencing
##or Detrending

#Option 1: DIFFERENCING
#Differencing involves taking differences between successive time series values; given that a Non-Stationary time-series
#presents with added normally distributed (random) errors (epsilons) across successive data-points, taking these errors constructs
#a Stationary time-series
#In other words, the time-series of a NonStationary time-series' epsilons is a Stationary time-series!

#difference time series to make stationary: the code below will substract from each value of X1 its previous lagged 
#of lag = 1 (lag function in the package "dplyr")
diff <- df$X1 - lag(df$X1,1)

#plot original and differenced time series in ACF plots
g1 <- ggAcf(df$X1, type = "correlation") + ggtitle("ACF for Original time-series")
g2 <- ggAcf(diff, type = "correlation") + ggtitle("ACF for differeced time-series")
grid.arrange(g1,g2)

#Option 2: DETRENDING
#Detrending involves removing a deterministic relationship with time from the time-series.
#So, the trended time-series is one that has a trend with time built into it - i.e. it can be described in a
#linear model with independent variable t (time) and coefficient B
#A trending time-series, therefore, can be described as Y(t) = B*t + epsilon
#Thus, detrending this function means arriving at: Y_det(t) = Yt - B*t = epsilon
#i.e., taking the residuals from that fitted linear model with time to be the new Stationary time-series.

#detrend time series to make stationary
detrended <- resid(lm(X2 ~ t, df))

#plot original and differenced time series in ACF plots
g1 <- ggAcf(df$X2, type = "correlation") + ggtitle("ACF for Original time-series")
g2 <- ggAcf(detrended, type = "correlation") + ggtitle("ACF for differeced time-series")
grid.arrange(g1,g2)

### BASIC MODEL TYPES

##Autoregressive Models AR(p)
#(p = number of AR lags)
#y(t) = Intercept + Coefficient1*y(t-1) + ... + Coefficientn*y(t-n) + epsilon

##Moving Average Models MA(q)
#(q = number of MA lags)

##Autoregressive Moving Average Models ARMA(p,q)
#Combination of AR and MA models

##Autoregressive Integrated Moving Average Models ARIMA(p,d,q)
#ARMA model with differencing 
#(d = number of differences to use)

##Decomposition Models
#They specifiy Y(t) as a combination of three components:
#a trend component (T)
#a seasonal component (S)
#an error component (E)
#So Y(t) = f(T(t) , S(t) , E(t))
#There are many different models to use when estimating these components. Here, we will see STL Model for its ease
#of use and flexibility

### FITTING MODELS WITH THE BOX JENKINS METHOD

##Steps
#Identify whether the time-series is Stationary
#Identify p, d, and q of the time-series by
        #Making the tim-series stationary through differencing/detrending to find d
        #Looking at ACF/PACF to find p and q
        #Using model fit diagnostics/selection (BMA, BIC, AIC, R-aquared, p-value) to select the best model to find
        #p, d, and q
                #(using model diagnostics/selection allows us to find p,d and q simultaneously)
#Check the model fit using the Ljung-Box test


### RUNNING IT IN A REAL WORLD DATASET

#Load data
ur <- read.csv("Mass Monthly Unemployment Rate.csv")

#Transform the type character DATE variable to a type date variable
ur$DATE <- as.Date(ur$DATE)
class(ur$DATE)

##Check for Stationarity

#plot the time series
ggplot(ur, aes(DATE, MAURN)) + geom_line()      #partial conclusion: seems NonStationary
#plot ACF
ggAcf(ur$MAURN, type = "correlation")           #partial conclusion: seems NonStationary
#unit root hypothesis test: Augmented Dickey-Fuller test, ADF test
adf.test(ur$MAURN)                              #conclusion: do not reject H0 -> it's NonStationary

##(Transform it so it is Stationary) and identify Model Parameters

#Fit AR Model with function auto.arima from "forecast" package
ar.model <- auto.arima(ur$MAURN , max.d = 0 , max.q = 0 , allowdrift=TRUE)
#we are specifying no differencing, no random noise and allowing for intercept
#you see, ARIMA with d=0 and q=0 is an AR(p)
#let us look at the output
ar.model
# the optimal AR model has p=1, so regression of y(t) ~ y(t-1)

#fit MA model with function auto.arima from "forecast" package
ma.model <- auto.arima(ur$MAURN , max.d = 0, max.p = 0, allowdrift=TRUE)
#now we are specifying no differencing, no correlation with previous values and allowing for intercept
#you see, ARIMA with d=0 and p=0 is an MA(q)
#let us look at the output
ma.model
#the optimal MA model is the one with q=5 (smaller std.err.), so moving average of previous 5 months

#fit ARMA model with function auto.arima from "forecast" package
arma.model <- auto.arima(ur$MAURN , max.d = 0, allowdrift=TRUE)
#now we are specifying no differencing and allowing for intercept
#you see, ARIMA with d=0 is an ARMA(p,q)
#let us look at the output
arma.model
#the optimal ARMA model has p=3 (ar3 has lowest s.e) and q=2 (ma2 has lowest s.e.)
#so y(t) is best predicted by correlation with 3 previous months + a moving average of the past 2 months

#fit ARIMA model with function auto.arima from "forecast" package
arima.model <- auto.arima(ur$MAURN , allowdrift=TRUE)
#now we are only allowing for intercept
#you see, ARIMA with p,q,d free to be chosen is an ARIMA(p,d,q)
#let us look at the output
arima.model
#the optimal ARIMA model has p = 4 , d = 1 , q = 2
#so y(t) is best predicted by correlation with 4 previous months + a moving average of last month + 2 months differencing

##Checking the Residuals of the Model Fit

#calculate residual of each model: remember that Stationary time-series will have white noise residuals, meaning
#randomly distributed, not autocorrelated

#Store residuals for each model
ar.residual <- resid(ar.model)
ma.residual <- resid(ma.model)
arma.residual <- resid(arma.model)
arima.residual <- resid(arima.model)

#plot PACF of each model residuals
ggAcf(ar.residual, type = "partial")
ggAcf(ma.residual, type = "partial")
ggAcf(arma.residual, type = "partial")
ggAcf(arima.residual, type = "partial")
#white noise residuals can´t be autocorrelated, so we expect to see almost no significant correlation values in these
#plots. plot 2, for example (ma model) shows signs of autocorrelation early on (small lag), which isn't "good".
#the arima model seems the most well behaved, with the smaller number of significant correlations amongst consecutive
#residuals (except for really far past lags)

#Run the Ljung-BOx test, for which:
#H0 = white noise residual and Ha = non-white noise residuals
#therefeore, we want a large p-value here to not reject the hypothesis that we have white-noise residuals
Box.test(ar.residual, type = "Ljung-Box" , lag=1)
Box.test(ma.residual, type = "Ljung-Box" , lag=1)
Box.test(arma.residual, type = "Ljung-Box" , lag=1)
Box.test(arima.residual, type = "Ljung-Box" , lag=1)
#All present high p-values: for all of these models, we cannot reject the null hypothesis that the residual are
#white noise only
#the last model, arima, is the one with the least amount of evidence to reject the null hypothesis: this is the 
#model for which we are most certain that the residuals are white-noise only
#Nevertheless, generally, in this case, we may proceed with any one of these four models

## Making a Forecast for Each Model

#with forecast() function from "forecast" package

ar.forecast <- forecast(ar.model , h=24 , level = 0.80)
ma.forecast <- forecast(ma.model , h=24 , level = 0.80)
arma.forecast <- forecast(arma.model , h=24 , level = 0.80)
arima.forecast <- forecast(arima.model , h=24 , level = 0.80)
#h: forecastinf for 24 months (database is in months)
#level: confidence interval

#plot forecast for each model
g1 <- autoplot(ar.forecast)
g2 <- autoplot(ma.forecast)
g3 <- autoplot(arma.forecast)
g4 <- autoplot(arima.forecast)
grid.arrange(g1,g2,g3,g4, nrow=2,ncol=2)

## Fit STL (Seasonal Trend Loess) Decomposition for each model
#STL models estimate the trend and seasonal components using LOESS regression, a regression method that applies more
#weight to data closer in time, relative to older observations

#There is an stl model built in R, but it requires a time-series oibject so, first, we will transform our dataset
#into a tim-series object
#need to specify frequency inside the seasonality period when doing this transformation (monthly data = 12)
ur.ts <- ts(ur$MAURN, frequency = 12)

#fit stl model using the stl() function from the "stats" package
stl.model <- stl(ur.ts, s.window = "periodic")
#s.window is the window of the seasonal component, "periodic" means that we want to estimate a seasonal component that
#has similar behaviour across different periods for whatever the window that the model choses as optimal

#plot the model
autoplot(stl.model)
#observe that the time-series is now broken down into three components
#T - the trend component (maybe could be explainde by macroeconomic factors?)
#S - the seasonal component, explained by regular seasonal changes in unemployment rates
#E - an error component, randomly distributed, not explained

#generate and plot forecast
stl.forecast <- forecast(stl.model, h=24, level=80)
autoplot(stl.forecast)


##Where to go Next
#Advanced time series models
        #ARCH, GARCH, etc. that model changing variance over time
#Vector Autoregression (VAR)
        #For multivariate i.e. multiple time series and modeling dependencies between them
#Machine Learning
        #How to do CV with time series
        #Neural networks for sequence data (LSTMs, etc.)
#Spatial Statistics
        #Generalize time dependence to spatial dependence in multiple dimensions
#Econometrics
        #Cointegration
        #Granger Causality
        #Serial correlation
        #Regression with time series data
#Bayesian time series














