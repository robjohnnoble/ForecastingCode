library(ggplot2)
library(readr)
library(reshape2)
library(dplyr)

###### 2.1

# Read and process the data:

Exercise_2_1 <- read_csv("Data/Exercise_2_1")

long_df <- melt(Exercise_2_1, id.vars = c("Year"), 
                variable.name = "Period", 
                value.name = "Sales")

long_df$Period_Numeric <- as.numeric(as.roman(as.character(long_df$Period)))

long_df$Time <- long_df$Year + long_df$Period_Numeric/13

long_df <- arrange(long_df, Time)

View(long_df)

# Plot the time series:

ggplot(long_df, aes(x = Time, y = Sales)) + 
  geom_line()

# Find averages by period and by year:

long_df <- long_df %>% group_by(Period) %>% mutate(PeriodMeanSales = mean(Sales)) %>% ungroup()

long_df <- long_df %>% group_by(Year) %>% mutate(YearMeanSales = mean(Sales)) %>% ungroup()

ggplot(long_df, aes(x = Year, y = YearMeanSales)) + 
  geom_point()

ggplot(long_df, aes(x = Period_Numeric, y = PeriodMeanSales)) + 
  geom_point()

# Moving averages:

df <- select(long_df, Year, Period_Numeric, Time, Sales)

df$ma3 <- as.numeric(stats::filter(df$Sales, filter = rep(1/3, 3), sides = 2))

df$ma6 <- as.numeric(stats::filter(df$Sales, filter = rep(1/6, 6), sides = 2))

df$ma13 <- as.numeric(stats::filter(df$Sales, filter = rep(1/13, 13), sides = 2))

long_df2 <- melt(df, id.vars = c("Year", "Period_Numeric", "Time"), 
                variable.name = "DataSet", 
                value.name = "Y")

ggplot(long_df2, aes(x = Time, y = Y, group = DataSet, colour = DataSet)) + 
  geom_line()

# Remove the trend:

df$SalesAdjustedForTrend <- df$Sales - df$ma13

ggplot(df, aes(x = Time, y = SalesAdjustedForTrend)) + 
  geom_line()

# Get the average seasonal component, after adjusting for trend:

sum_df <- group_by(df, Period_Numeric) %>% summarise(MeanSalesAdjustedForTrend = mean(SalesAdjustedForTrend, na.rm=TRUE)) %>% ungroup()

ggplot(sum_df, aes(x = Period_Numeric, y = MeanSalesAdjustedForTrend)) + 
  geom_line()

# Same thing using TTR package:

library(TTR)

Exercise_2_1_ts <- ts(long_df$Sales, frequency=13, start=c(1995,1))

plot.ts(Exercise_2_1_ts)

Exercise_2_1components <- decompose(Exercise_2_1_ts)

plot(as.numeric(Exercise_2_1components$seasonal), type = "l")
lines(sum_df$MeanSalesAdjustedForTrend, col = "orange")

###### 2.2

x = c(1.6, 0.8, 1.2, 0.5, 0.9, 1.1, 1.1, 0.6, 1.5, 0.8, 0.9, 1.2, 0.5, 1.3, 0.8, 1.2)

plot(x, type = "l")
points(x)

nextx <- lead(x, 1)

plot(nextx ~ x, xlim = c(0, 1.7), ylim = c(0, 1.7))
xx <- seq(0, 2, length = 100)
lines(1*xx ~ xx, lty = 2)
lines(2-xx ~ xx, lty = 2)

acf(x)

acf(x, plot = FALSE)$acf

# Use formula:

N <- length(x)
x_bar <- mean(x)

r1_top <- sum((x - x_bar) * (nextx - x_bar), na.rm = TRUE)
r1_bottom <- sum((x - x_bar)^2)

r1_top / r1_bottom

###### 2.4

acf_values <- c(0.02, 0.05, -0.09, 0.08, -0.02, 0, 0.12, 0.06, 0.02, -0.08)

plot(acf_values, ylim = c(-0.15, 0.15))

abline(h = 2 / sqrt(400), lty = "dashed")
abline(h = -2 / sqrt(400), lty = "dashed")

###### 2.5

t <- seq(0, 2 * pi, length = 12)
S <- sin(t)

t <- c(t, t + 2*pi, t + 4*pi, t + 6*pi)
S <- rep(S, times = 4)

plot(S ~ t, type = "l")

a <- -1
b <- 0.1
epsilon <- rnorm(length(t), 0, 0.3)

# model a:

X <- a + b*t + S + epsilon

plot(X ~ t, type = "l")

X_diff <- X - lag(X, 12)

theoretical_X_diff_mean <- b * 2 * pi

par(mfrow = c(2, 1))
plot(epsilon ~ t, type = "l", ylim = c(-1, 1))
abline(h = 0, lty = "dashed")
plot(X_diff ~ t, type = "l", ylim = c(theoretical_X_diff_mean-1, theoretical_X_diff_mean+1))
abline(h = theoretical_X_diff_mean, lty = "dashed")

sqrt(var(epsilon))
sqrt(var(X_diff, na.rm = TRUE))

# model b:

X2 <- (a + b*t) * S + epsilon

par(mfrow = c(1, 1))

plot(X2 ~ t, type = "l")

X2_diff <- X2 - lag(X2, 12)

plot(X2_diff ~ t, type = "l")
lines(12*b*S ~ t, lty = "dashed")

X2_diff2 <- X2_diff - lag(X2_diff, 12)

plot(X2_diff2 ~ t, type = "l")
abline(h = 0, lty = "dashed")




