library(lubridate)
library(forecast)

RunningData <- read.csv("RunningData.csv")

# convert to a ts object:
my_ts <- ts(RunningData$Distance, 
            frequency = 7)

acf(my_ts)
# acf shows seasonality with period 7

X <- my_ts - stats::lag(my_ts, 7)
plot(X)
acf(X)
# after seasonal differencing, acf still shows seasonality with period 7
# so try including a seasonal AR term

###### Backtesting:

train_size <- floor(0.6 * length(my_ts))

train_ts <- window(my_ts, end = time(my_ts)[train_size])
test_ts  <- window(my_ts, start = time(my_ts)[train_size + 1])

model_arima <- arima(train_ts,
                     order = c(1, 0, 0),
                     seasonal = list(order = c(1, 1, 0), period = 7))

h <- length(test_ts)
forecasted_values <- forecast(model_arima, h = h)

plot(forecasted_values, xlab = "date", ylab = "distance")
lines(test_ts, col = "red")  # data

accuracy(forecasted_values, test_ts)

###### Try an ARIMAmodel:

model_arima <- arima(my_ts, 
                     order = c(1, 0, 0), 
                     seasonal = list(order = c(1, 1, 0), period = 7))
# this is an ARIMA(1,0,0)x(1,1,0)[7] model

print(model_arima)

plot(residuals(model_arima))

acf(residuals(model_arima))
# acf of residuals shows no significant autocorrelation

###### Backtesting with rolling startpoint:

errors <- tsCV(my_ts, function(y, h) {
  fit <- arima(y, order = c(1,0,0),
               seasonal = list(order = c(1,1,0), period = 7))
  forecast(fit, h = h)
}, h = 1)

plot(errors, main = "Rolling Forecast Errors", ylab = "Error", xlab = "Time")
abline(h = 0, col = "red")

fitted_vals <- my_ts - errors

plot(my_ts, col = "black", lwd = 2,
     main = "Rolling Forecast vs Actual",
     ylab = "Value")

lines(fitted_vals, col = "blue")

legend("topleft",
       legend = c("Actual", "Rolling Forecast"),
       col = c("black", "blue"),
       lty = 1)

###### Forecasting:

forecasted_values <- forecast(model_arima, 25)

plot(forecasted_values, xlab = "date", ylab = "distance")

###### Auto ARIMA:

model_auto <- auto.arima(my_ts, seasonal = TRUE,
                         stepwise = FALSE,
                         approximation = FALSE)
# this suggests instead an ARIMA(0,0,1)x(2,1,0)[7] model

print(model_auto)

model_auto$coef

plot(residuals(model_auto))

acf(residuals(model_auto))
# acf of residuals shows no significant autocorrelation

forecasted_values <- forecast(model_auto, h = 25)

plot(forecasted_values, xlab = "date", ylab = "distance")





