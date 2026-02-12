# Code adapted from "Using R for Time Series Analysis"
# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
# Licensed under a Creative Commons Attribution 3.0 License
# https://creativecommons.org/licenses/by/3.0/

library(TTR)
library(forecast)
library(aTSA)

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))

pdf(file = "Plots/kings.pdf", width = 4, height = 3.2)
par(mar = c(4,4,1,1))
plot.ts(kingstimeseries, xlab = "King number", ylab = "Age at death")
dev.off()

pdf(file = "Plots/births.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(birthstimeseries, xlab = "Time", ylab = "Number of births")
dev.off()

pdf(file = "Plots/souvenirs.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(souvenirtimeseries, xlab = "Time", ylab = "Sales")
dev.off()

pdf(file = "Plots/souvenirs_log.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(souvenirtimeseries, xlab = "Time", ylab = "Sales", log = "y")
dev.off()

# simple moving average of order 3:
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
# this is equivalent to as.numeric(stats::filter(kingstimeseries, filter = rep(1/3, 3), sides = 1))

pdf(file = "Plots/kings_smoothed1.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(kingstimeseriesSMA3, col = "red", ylim = c(10, 85), xlab = "King number", ylab = "Age at death")
lines(kingstimeseries, col = "grey80")
dev.off()

# simple moving average of order 8:
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)

pdf(file = "Plots/kings_smoothed2.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(kingstimeseriesSMA3, col = "red", ylim = c(10, 85), xlab = "King number", ylab = "Age at death")
lines(kingstimeseries, col = "grey80")
lines(kingstimeseriesSMA8, col = "blue")
dev.off()

# *symmetric* simple moving average of order 3:
kingstimeseriesSMA3_sym <- as.numeric(stats::filter(kingstimeseries, filter = rep(1/3, 3), sides = 2))

pdf(file = "Plots/kings_smoothed1_sym.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(kingstimeseriesSMA3_sym, col = "red", ylim = c(10, 85), xlab = "King number", ylab = "Age at death")
lines(kingstimeseries, col = "grey80")
dev.off()

# *symmetric* simple moving average of order 8:
kingstimeseriesSMA8_sym <- as.numeric(stats::filter(kingstimeseries, filter = rep(1/8, 8), sides = 2))

pdf(file = "Plots/kings_smoothed2_sym.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(kingstimeseriesSMA3_sym, col = "red", ylim = c(10, 85), xlab = "King number", ylab = "Age at death")
lines(kingstimeseries, col = "grey80")
lines(kingstimeseriesSMA8_sym, col = "blue")
dev.off()

# exponential moving average of order 3:
kingstimeseriesEMA3 <- EMA(kingstimeseries,n=3)

pdf(file = "Plots/kings_smoothed1_exp.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(kingstimeseriesEMA3, col = "red", ylim = c(10, 85), xlab = "King number", ylab = "Age at death")
lines(kingstimeseries, col = "grey80")
dev.off()

# exponential moving average of order 8:
kingstimeseriesEMA8 <- EMA(kingstimeseries,n=8)

pdf(file = "Plots/kings_smoothed2_exp.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(kingstimeseriesEMA3, col = "red", ylim = c(10, 85), xlab = "King number", ylab = "Age at death")
lines(kingstimeseries, col = "grey80")
lines(kingstimeseriesEMA8, col = "blue")
dev.off()

pdf(file = "Plots/kings_residuals.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(kingstimeseries - kingstimeseriesEMA3, ylim = c(-20, 20), xlab = "King number", ylab = "Residual variation")
dev.off()

# exponential moving average of order 8 with alpha = :
#kingstimeseriesEMA8_01 <- EMA(kingstimeseries, n=8, ratio = 0.1)
kingstimeseriesEMA8_05 <- EMA(kingstimeseries, n=8, ratio = 0.5)

pdf(file = "Plots/kings_smoothed3_exp.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(kingstimeseries, col = "grey80", ylim = c(10, 85), xlab = "King number", ylab = "Age at death")
#lines(kingstimeseriesEMA8_01, col = "black")
lines(kingstimeseriesEMA8, col = "blue")
lines(kingstimeseriesEMA8_05, col = "red")
dev.off()


# The expsmooth function (from aTSA) produces a curve that is offset by two timepoints, 
# which seems inconsistent with the mathematical definition in the manual
kingstimeseries_expsmooth <- expsmooth(kingstimeseries, plot = FALSE)
kingstimeseries_expsmooth_05 <- expsmooth(kingstimeseries, alpha = 0.5, plot = FALSE)

plot.ts(kingstimeseries, col = "grey80", ylim = c(10, 85), xlab = "King number", ylab = "Age at death")
lines(as.numeric(kingstimeseries_expsmooth$estimate)[2:42], col = "blue")
lines(as.numeric(kingstimeseries_expsmooth_05$estimate)[2:42], col = "red")

#######

kingstimeseries_differenced <- kingstimeseries - stats::lag(kingstimeseries)

pdf(file = "Plots/kings_differences.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot.ts(kingstimeseries_differenced, ylim = c(-40, 40), xlab = "King number", ylab = "First difference")
dev.off()

#######

# decompose births time series:
birthstimeseriescomponents <- decompose(birthstimeseries)

pdf(file = "Plots/births_decomposed.pdf", width = 7, height = 5)
plot(birthstimeseriescomponents)
dev.off()

# seasonally adjusting:

birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal

pdf(file = "Plots/births_no_season.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(birthstimeseriesseasonallyadjusted)
dev.off()

birthstimeseriestrendadjusted <- birthstimeseries - birthstimeseriescomponents$trend

pdf(file = "Plots/births_no_trend.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(birthstimeseriestrendadjusted)
dev.off()

birthstimeseriesnoiseadjusted <- birthstimeseries - birthstimeseriescomponents$random

pdf(file = "Plots/births_no_noise.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(birthstimeseriesnoiseadjusted)
dev.off()

########

# Different seasonality models:

t <- seq(0, 20 * pi, length = 20 * 20)
S <- sin(t)

plot(S ~ t, type = "l")

a <- 1
b <- 0.01
epsilon <- rnorm(length(t), 0, 0.3)

# model a:
X1 <- a + b*t + S + epsilon

# model b:
X2 <- (a + b*t) * S + epsilon

# model c:
X3 <- exp(X1)

t_plot <- 1:length(S)

pdf(file = "Plots/seasonal_models.pdf", width = 7, height = 3.2) 
par(mar = c(4,4,1,1))
par(mfrow = c(1, 2))
plot(X1 ~ t_plot, type = "l", col = "grey", ylab = "X_t", xlab = "t")
lines(a + b*t + S ~ t_plot, col = "red")
lines(a + b*t ~ t_plot, col = "blue", lty = 2)
# plot(X2 ~ t, type = "l", col = "grey")
# lines(a * S ~ t, col = "red")
plot(X3 ~ t_plot, type = "l", col = "grey", ylab = "X_t", xlab = "t")
lines(exp(a + b*t + S) ~ t_plot, col = "red")
lines(exp(a + b*t) ~ t_plot, col = "blue", lty = 2)
# plot(X3 ~ t, type = "l", log = "y", col = "grey")
# lines(exp(a + S) ~ t, col = "red")
dev.off()

X4 <- S + epsilon

pdf(file = "Plots/seasonal_model1_only.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(X4[1:120] ~ t_plot[1:120], type = "l", ylab = "X_t", xlab = "t")
abline(h = 0.5, lty = 2)
abline(h = -0.5, lty = 2)
dev.off()

########

x <- rnorm(100)

pdf(file = "Plots/acf_random.pdf", width = 5, height = 5)
par(mfrow = c(2, 1))
par(mar = c(4,4,1,1))
plot(x, xlab = "t", ylab = "x_t", type = "l")
# approximately stationary example
acf(x, lag.max = 20, main = "")
dev.off()

pdf(file = "Plots/acf_kings.pdf", width = 5, height = 5)
par(mfrow = c(2, 1))
par(mar = c(4,4,1,1))
plot.ts(kingstimeseries, xlab = "King number", ylab = "Age at death")
# approximately stationary example
acf(kingstimeseries, lag.max = 20, main = "")
dev.off()

Box.test(kingstimeseries, lag=20, type="Ljung-Box")

pdf(file = "Plots/acf_births.pdf", width = 5, height = 5) 
par(mfrow = c(2, 1))
par(mar = c(4,4,1,1))
plot.ts(birthstimeseries, xlab = "Time", ylab = "Number of births")
# non-stationary example
acf(birthstimeseries, lag.max = 36, main = "", xlab = "Lag (years)")
dev.off()

pdf(file = "Plots/acf_souvenirs.pdf", width = 5, height = 5) 
par(mfrow = c(2, 1))
par(mar = c(4,4,1,1))
plot.ts(souvenirtimeseries, xlab = "Time", ylab = "Sales")
# seasonal series example
acf(souvenirtimeseries, lag.max = 24, main = "", xlab = "Lag (years)")
dev.off()

pdf(file = "Plots/acf_souvenirs_logged.pdf", width = 5, height = 5) 
par(mfrow = c(2, 1))
par(mar = c(4,4,1,1))
plot.ts(souvenirtimeseries, xlab = "Time", ylab = "Sales", log = "y")
# seasonal series example
acf(log10(souvenirtimeseries), lag.max = 24, main = "", xlab = "Lag (years)")
dev.off()

x1 <- souvenirtimeseries
x1[20] <- souvenirtimeseries[20] * 100

pdf(file = "Plots/acf_souvenirs_logged_with_outlier.pdf", width = 5, height = 5)
par(mfrow = c(2, 1))
par(mar = c(4,4,1,1))
plot.ts(x1, xlab = "Time", ylab = "Sales", log = "y")
acf(log10(x1), lag.max = 24, main = "", xlab = "Lag (years)")
dev.off()

