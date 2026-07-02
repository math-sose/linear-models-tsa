# Time-series library
library(TSA)
library(tseries)
library(forecast)
library(astsa)

# Daily USD/HKD exchange rate from January 1, 2005 to March 7, 2006
data("usd.hkd")
forex = ts(usd.hkd$hkrate)

# perform initial tests
adf.test(forex)
Box.test(forex, type="Ljung")

# check ACF, PACF
acf(forex)
pacf(forex)

# from previous result, use ARMA(2,1)
arma21 = arima(forex, order=c(2,0,1))
arma21$coef
# forecast 5-step ahead
arma21.p = predict(arma21, n.ahead=5)

# alternative function for estimation and forecasting
# forecast() only works when Arima() is used
arma21.2 = Arima(forex, order=c(2,0,1))
arma21.2$coef
arma21.p2 = forecast(arma21.2, h=5)

# residual diagnostics
# Ljung-Box have issues with degrees of freedom
# jarque.bera.test() is used to check for skewness/kurtosis
tsdiag(arma21)
jarque.bera.test(residuals(arma21)) 

###############################

# annual number of USA union strikes, 1951-1980
# perform initial tests and transformations
strikes = read.csv("strikes.csv")
strikes.l = log(ts(strikes))
strikes.l.d = diff(strikes.l)
strikes.l.d2 = diff(strikes.l.d)
Box.test(strikes.l.d2, type="Ljung-Box")
adf.test(strikes.l.d2)

# determine order using ACF/PACF
acf(strikes.l.d2)
pacf(strikes.l.d2)

# estimate coefficients
# methods: ARMA, ARIMA, Yule-Walker
# note that ARMA estimates a mean, while ARIMA assumes zero-mean
ar2 = Arima(strikes.l.d2, order=c(2,0,0))
ari2 = Arima(strikes.l, order=c(2,2,0))
ar2.nomean = Arima(strikes.l.d2, order=c(2,0,0), include.mean=F)
ar2.yw = ar.yw(strikes.l.d2, order=2)

# forecast 5-step ahead, differenced data
ar2.p = forecast(ar2, h=5)
plot(ar2.p)

# forecast 5-step ahead, original data
ar2.p2 = forecast(ari2, h=5)
plot(ar2.p2)

# residual diagnostics
tsdiag(ar2) 
jarque.bera.test(residuals(ar2))

###############################

# Monthly Airline Passenger Numbers 1949-1960
air = AirPassengers

# perform initial transformations
air.l = log(air)
air.l.d = diff(air.l)
plot(cbind(air, air.l,air.l.d))

# perform initial tests
Box.test(air.l.d,type="Ljung")
adf.test(air.l.d)

# do seasonal differencing
# use default frequency as s
air.l.d.12 = diff(air.l.d,12)
plot(cbind(air.l,air.l.d, air.l.d.12))

# determine order
# note that lag 1 = step 12
acf(air.l.d) 
acf(air.l.d, lag=100)
pacf(air.l.d, lag=100)

# fit using arima
sarima.ar1 = Arima(air.l, order=c(1,1,1), seasonal=list(order=c(1,1,0), period=12))
sarima.ma1 = Arima(air.l, order=c(0,1,1), seasonal=list(order=c(1,1,0), period=12))
sarima.arma11 = Arima(air.l, order=c(1,1,0), seasonal=list(order=c(1,1,0), period=12))

# selecting best model
c(sarima.ar1$aic, sarima.ma1$aic, sarima.arma11$aic)

# fit using Arima, the forecast
# if period is omitted, default is frequency
sarima.model = Arima(air.l,order=c(0,1,1),seasonal=list(order=c(1,1,0),period=12))
sfor = forecast(sarima.model, h=12)
plot(sfor)
sfor$mean

# input is differenced data
sarima.model.d = Arima(air.l.d,order=c(0,0,1),season=c(1,1,0))
sfor2 = forecast(sarima.model.d, h=12)
plot(sfor2)

# input is seasonal differenced data
sarima.model.d.12 = Arima(air.l.d.12,order=c(0,0,1),season=c(1,0,0), include.mean=F)
sfor3 = forecast(sarima.model.d.12, h=12)
plot(sfor3)