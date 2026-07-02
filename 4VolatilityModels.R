# Time-series library
library(TSA)
library(tseries)
library(forecast)
library(astsa)
library(fGarch)

# Daily USD/HKD exchange rate from January 1, 2005 to March 7, 2006
# use result from previous modelling
data("usd.hkd")
forex = ts(usd.hkd$hkrate)
arma21 = arima(forex, order=c(2,0,1))

# get the residuals after fitting ARMA(2,1)
res.arma21 = residuals(arma21)

# check the residuals
acf(res.arma21)
pacf(res.arma21)
Box.test(res.arma21, type="Ljung")

# check the squared residuals
acf(res.arma21^2)
pacf(res.arma21^2)
Box.test(res.arma21^2, type="Ljung")

# two-pass estimation
vol.ar1 = Arima(res.arma21^2, order=c(1,0,0))
vol.ar1$coef

# fit ARMA(p,q)-ARCH(1) model
vol = garchFit(~garch(1,0),data=forex) #arma(0,0)
arma21.vol = garchFit(~arma(2,1)+garch(1,0),data=forex) #arma(2,1)
summary(vol)
summary(arma21.vol)

# refine model
arma11.vol = garchFit(~arma(1,1)+garch(1,0),data=forex)
summary(arma11.vol)
arma11.vol@fit$coef
arma11.vol@fit$se.coef

# check standardized residuals
fit.vol = volatility(arma11.vol)
fit.vol.sr = residuals(arma11.vol) / volatility(arma11.vol)
plot(fit.vol, type="l")
plot(fit.vol.sr, type="l")

# check standardized residuals
acf(fit.vol.sr)
pacf(fit.vol.sr)
Box.test(fit.vol.sr, type="Ljung", lag=7)

# check standardized squared residuals
acf(fit.vol.sr^2)
pacf(fit.vol.sr^2)
Box.test(fit.vol.sr^2, type="Ljung")

# forecast values
forex.p = predict(arma11.vol, n.ahead=5, plot=T)
forex.p