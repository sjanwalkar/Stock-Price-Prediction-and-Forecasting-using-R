# Arima Model

rm(list=ls())

# Load the dataset

google_data <- getSymbols(Symbols = "GOOGL", src = "yahoo", from = "2000-01-01", 
                        to = Sys.Date(), auto.assign = FALSE)

google_data <- Cl(google_data)

# STOCK CHARTING

#in order to begin analysing stock, here's the charting plus some technical indicators such as Simple Moving Average (20 and 100), Bollinger bands (20, sd = 1), Relative Strength Index (14 days), and Moving Average Convergence Divergence (12, 25) as the technical analysis before forecasting.
chart_Series(google_data, col = "black")
add_SMA(n = 100, on = 1, col = "red")
add_SMA(n = 20, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_BBands(n = 20, maType = "SMA", sd = 1, on = -1)
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)

# Checking whether data is Stationary or Not
plot(google_data)

# Transforming the data with Log

plot(log(google_data)) # Adjusting the variance


# Checking the Autocorrelation and Partial Autocorrelation Function
acf_google = acf(google_data, lag.max = 320)

pacf_google=pacf(google_data, lag.max = 320)

#Given by the ACF correlogram, we can see that the data shows strong and significant autocorrelation up to lag 320. 
#For the PACF, significant autocorrelations appear in lag 3, 4, 38, 40-ish and then the autocorralation starts oscillating aroung the 0. 
#This is the sign of certain trend, but we are unsure wether the data has seasonality or not, given that the PACF does not have any significant seasonal pattern. 
#Therefore we conclude that indf stock price is non-stationary.

# TO make our data stationary we should differenciate it to adjust the mean component i.e. all mean are equal
plot(diff(log(google_data))) # Adjusting the mean

google_data = diff(log(google_data))
google_data = na.locf(google_data, na.rm = TRUE,
                      fromLast = TRUE)



# Testing the Stationarity using Augmented Dickey Fuller Test.
# Null Hypothesis = Non-Satationary
# Alternative Hyp = Stationary
library(tseries)
adf <- adf.test(google_data, alternative = c("stationary", "explosive"), 
                k = 0)
adf
# Hence as p-value<0.05, we RJ null and conclude that our data is Stationary.

# Plotting ACF and PACF Curves

acf_google  = acf(google_data) # Determining the value of q

pacf_google = pacf(google_data) # Determining the value of p


# Build the ARIMA Model
#R provides simple and automatic way to generate appropriate ARIMA(p, d, q) model using auto.arima() function in forecast package.
#Here we pass in our train data, difference (d = 1),stationary = TRUE
library(forecast)
set.seed(100)
arima_model <- auto.arima(google_data, stationary = TRUE, ic = c("aicc", "aic", "bic"), 
                          trace = TRUE)

summary(arima_model)

checkresiduals(arima_model) ###diagnostic cheking

# Fitting the model and forecasting
arima <- arima(google_data, order = c(4, 0, 2))
summary(arima)

forecast_google <- forecast(arima, h = 365)
plot(forecast_google)

checkresiduals(arima)
# Here our forecast for 365 days ahead shows straight line. This is due to nature of arima forecasting tends to be mean reversion. 
#The Ljung Box test shows that the model residuals are non-autocorrelated, suggesting there's no heterocedasticity problem and the model is good, otherwise we might consider GARCH model.


pred = predict(arima, n.ahead = 365)
pred1 = 2.718^pred$pred

plot(google_data)
lines(pred$pred, col="blue")
lines(pred$pred+2*pred$se, col="red")
lines(pred$pred-2*pred$se, col="red")

ts.plot(as.ts(google_data), pred1, log='y',col= c(4,2),lty=c(1,3))
pred1

fore = forecast()