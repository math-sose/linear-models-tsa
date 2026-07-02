##########################################################
## Basic Time Series Analysis
##########################################################

# Time-series library
library("TSA")
library("tseries")

# simulated data
data("ar2.s")
data("ma1.1.s")
z = ts(rnorm(120))

# check data
ar2.s

# plot time series data
plot(ar2.s)
plot(ma1.1.s)

# change frequency; even if frequency was changed, index is unchanged
## set frequency to 10 per unit time
z1 = ts(z, frequency=10) 
## set frequency to monthly, from Dec 2012 to Mar 2023
z2 = ts(z, start=c(2012,12), end=c(2023,3),frequency=12) 
plot(z)
plot(z1)
plot(z2)

# check for autocorrelation
# default is lag = 1
Box.test(ar2.s, type="Ljung")
Box.test(ma1.1.s, type="Ljung")
Box.test(z, type="Ljung", lag=3) # test for lag-3 correlation

# test for stationarity (AR)
adf.test(ar2.s)
adf.test(ma1.1.s)
adf.test(z)

# compute for ACF
acf(ar2.s)
acf(ma1.1.s)
acf(z)

# compute for PACF
pacf(ar2.s)
pacf(ma1.1.s)
pacf(z)

# fit ARMA models
# order = AR lag, integration, MA lag 
arima(ar2.s, order=c(1,0,0)) # test AR(1)
arima(ar2.s, order=c(2,0,0)) # test AR(2)
arima(ar2.s, order=c(9,0,0)) # test AR(9)

arima(ma1.1.s, order=c(0,0,1)) # test MA(1)
arima(ma1.1.s, order=c(0,0,5)) # test MA(5)
arima(ma1.1.s, order=c(0,0,14)) # test MA(14)

##########################################################
## Modelling with Data
##########################################################

# Quarterly S&P Composite Index, 1936Q1 - 1977Q4.
data("SP")

# test for stationarity (AR)
adf.test(SP)

# perform pre-processing
SP.ln = log(SP) # take log transformation
SP.ln.d = diff(SP.ln) # take first difference of log
plot(SP.ln.d)

# perform stationarity test for differenced data
adf.test(SP.ln.d)

# check autocorrelation
Box.test(SP.ln.d, type="Ljung")

# Daily USD/HKD exchange rate from January 1, 2005 to March 7, 2006
data("usd.hkd")
forex = ts(usd.hkd$hkrate)

# perform initial tests
adf.test(forex)
Box.test(forex, type="Ljung")

# check ACF, PACF
acf(forex)
pacf(forex)

# check models, lower AIC is better
arima(forex, order=c(0,0,1)) # test MA(1)
arima(forex, order=c(2,0,0)) # test AR(2)
arima(forex, order=c(2,0,1)) # test ARMA(2,1)