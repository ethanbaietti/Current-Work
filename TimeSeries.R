if(!require(quantmod)) install.packages("quantmod")
if(!require(forecast)) install.packages("forecast")
if(!require(xlsx)) install.packages("xlsx")
if(!require(tseries)) install.packages("tseries")
if(!require(timeSeries)) install.packages("timeSeries")
if(!require(dplyr)) install.packages("dplyr")
if(!require(fGarch)) install.packages("fGarch")

library(quantmod)
library(forecast)
library(xlsx)
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)
library(astsa)

getSymbols("XLY", from = "2015-01-01", src = "yahoo")
sma_200 <- SMA(Cl(XLY), n = 200)

plot(Cl(XLY))
lines(sma_200, col = "red", lwd = 6)

acf2(Cl(XLY))

xly_close <- log(XLY$XLY.Close)
xly_close_d <- diff(xly_close)
xly_close_d <- xly_close_d[!is.na(xly_close_d)]

acf2(xly_close)

sarima(xly_close, 1,0,1)


acf(xly_close, na.action = na.pass)
pacf(xly_close, na.action = na.pass)
ts.plot(xly_close)

print(adf.test(xly_close[-1]))

###returns <- eu_stocks[-1,] / eu_stocks[-1860,] - 1


mod1 <- auto.arima(xly_close, lambda = "auto")
mod2 <- sarima(xly_close, 1,1,2)
mod3 <- auto.arima(xly_close, seasonal = TRUE)
summary(mod1)
summary(mod3)
acf2(mod1$residuals)
acf2(mod3$residuals)

autoplot(xly_close)
gglagplot(xly_close)
ggAcf(xly_close)
ggseasonplot(xly_close, continuous = T)

#Cross Validation

farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

e1 <- tsCV(xly_close, farima, h = 1)
mse_arima <- mean(e^2, na.rm = T)

arima_fc <- farima(train, h = 19)
accuracy(arima_fc, test)

fit <- auto.arima(train, stepwise = FALSE)
acf2(fit$residuals)
checkresiduals(fit)
fit_fc <- forecast(fit, h = 19)
accuracy(fit_fc, test)
summary(fit)

# Ljung-Box test of the differenced series
Box.test(xly_close_d, lag = 10, type = "Lj")

#Naive Forecast
fc_xly <- naive(xly_close, h = 20)
autoplot(fc_xly)
summary(fc_xly)

checkresiduals(fc_xly)

## Accuracy , Train / Test Set

train <- window(xly_close, end = "2020-03-31")
test <- window(xly_close, start = "2020-04-01")

naive_fc <- naive(train, h = 19)
mean_fc <- meanf(train, h = nrow(xly_close) - nrow(train))

accuracy(naive_fc, test)
accuracy(mean_fc, test)

#Time Series Cross Validation
e <- tsCV(xly_close, forecastfunction = naive, h = 2)
mse <- colMeans(e^2, na.rm = TRUE)
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point() +scale_x_continuous(0:1)

#Simple Exponetial Smoothing
fc_ses <- ses(xly_close, h = 8)
summary(fc_ses)
autoplot(fc_ses)
autoplot(fc_ses) + autolayer(fitted(fc_ses))

# SES VS Naive
fc_ses_t <- ses(train, h = 19)
accuracy(fc_ses_t, test)

# Holt Function - Exponetial Smoothing with Trend
fc_holt <- holt(XLY$XLY.Close, h = 20)
summary(fc_holt)
autoplot(fc_holt)
checkresiduals(fc_holt)

#Holt_Winter Forecast - Exponetial Smoothing for Trend and Seasonality
#fc_hw <- hw(xly_close, seasonal = "additive" , h = 5)

#Errors, Trend, Sesonality (ETS) Model
fit_ets <- ets(XLY$XLY.Close)
checkresiduals(fit_ets)
autoplot(forecast(fit_ets, h = 50))


#Dyamic Regression




###Log Returns Site

breakpoint = floor(nrow(xly_close_d)*.8)
acf2(xly_close_d)


Actual_series = xts(0,as.Date("2014-11-25", "%Y-%m-%d"))              
forecasted_series = data.frame(Forecasted = numeric())

for (i in breakpoint:nrow(xly_close_d)-1)) {
  stock_train = xly_close_d[1:i,]
  stock_test = xly_close_d[i+1:nrow(xly_close_d)]
  fit = auto.arima(stock_train, lamda = "auto")
  summary(fit)
  acf2(fits$residuals)
  arima.forecast = forcast
}


barChart(XLY)

####NEE

getSymbols("NEE", from = "2018-01-01", src = "yahoo")
nee_close <- NEE$NEE.Close
plot(nee_close)
ndiffs(nee_close)

acf2(nee_close)

nee_close <- log(nee_close)
acf2(nee_close)
print(adf.test(nee_close[-1]))

mod_nee <- auto.arima(nee_close)
acf2(mod_nee$residuals)
checkresiduals(mod_nee)

mod_nee_s <- auto.arima(nee_close, seasonal = TRUE)
summary(mod_nee_s)
acf2(mod_nee_s$residuals)
checkresiduals(mod_nee_s)
print(adf.test(mod_nee_s$residuals))
mod_nee_s %>%  forecast(h = 100) %>% autoplot()
Box.test(nee_close, ty)
