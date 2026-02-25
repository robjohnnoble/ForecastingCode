k <- 0:20

alpha1 <- 0.75
acf1 <- alpha1^k

alpha2 <- -0.75
acf2 <- alpha2^k

k1 <- k - 0.1
k2 <- k + 0.1

pdf(file = "Plots/AR1_acf.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(acf1 ~ k1, type="h", ylim = c(-1, 1), col = "blue", xlab = "k", ylab = "acf")
lines(acf2 ~ k2, type="h", col = "red")
dev.off()

########## An AR(3) process:

# The ac.f of the AR(3) process
# X_t = -X_{t-1}/4 + X_{t-2}/4 + X_{t-3}/16 + Z_t

max_t <- 1000
Z <- rnorm(max_t, 0, 1)

X <- as.vector(rep(NA, max_t))
X[1] <- 0
X[2] <- 0
X[3] <- 0
a1 <- -1/4
a2 <- 1/4
a3 <- 1/16
for(i in 4:max_t) X[i] <- a1*X[i-1] + a2*X[i-2] + a3*X[i-3] + Z[i]

t <- 1:max_t
plot(X ~ t, type = "l")
  
acf(X, plot = FALSE) # sample ac.f

A2 <- -12/39
A1 <- 7/26
rho_k <- function(k) A1/2^k + A2/(-4)^k + (1-A1-A2)/(-2)^k

k <- 0:40

pdf(file = "Plots/AR3_acf.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(X, lag.max = 20, main = "") # sample ac.f
points(rho_k(k) ~ k, col = "red") # theoretical ac.f
dev.off()

####### Another AR(3) process:

max_t <- 1e6
Z <- rnorm(max_t, 0, 1)

X <- as.vector(rep(NA, max_t))
X[1] <- 0
X[2] <- 0
X[3] <- 0
a1 <- 1/4
a2 <- -1/2
a3 <- -1/4
for(i in 4:max_t) X[i] <- a1*X[i-1] + a2*X[i-2] + a3*X[i-3] + Z[i]

pdf(file = "Plots/AR3_acf_alt.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(X, lag.max = 20, main = "", ci = 0) # sample ac.f
dev.off()

####### An AR(1) process:

max_t <- 50
Z <- rnorm(max_t, 0, 1)

X <- as.vector(rep(NA, max_t))
mu <- 2
a1 <- 0.5
X[1] <- mu
for(i in 2:max_t) X[i] <- a1*(X[i-1] - mu) + Z[i] + mu

t <- 1:max_t

pdf(file = "Plots/AR1_sample.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(X ~ t, type = "l", ylab = "X_t")
points(X ~ t)
abline(h = mean(X), lty = 2)
abline(h = mu, lty = 2, col = "red")
dev.off()

k <- 0:20

pdf(file = "Plots/AR1_sample_acf.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(X, lag.max = 20, main = "") # sample ac.f
points(a1^k ~ k, col = "grey")
points(x = 1, y = 0.5, col = "red")
dev.off()

model <- arima(X, order = c(1, 0, 0))
print(model)

plot(residuals(model))

####### An MA(2) process:

max_t <- 50
Z <- rnorm(max_t, 0, 1)

X <- as.vector(rep(NA, max_t))
mu <- 2
b1 <- 0.5
X[1] <- mu
for(i in 2:max_t) X[i] <- Z[i] + b1*Z[i-1] + mu

t <- 1:max_t

model <- arima(X, order = c(0, 0, 1))
print(model)
mu_est <- model$coef[2]

pdf(file = "Plots/MA1_sample.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(X ~ t, type = "l", ylab = "X_t")
points(X ~ t)
abline(h = mu_est, lty = 2)
abline(h = mu, lty = 2, col = "red")
dev.off()

k <- 0:20

pdf(file = "Plots/MA1_sample_acf.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(X, lag.max = 20, main = "") # sample ac.f
points(x = 0, y = 1, col = "grey")
points(x = 1, y = b1/(1+b1^2), col = "grey")
points(x = 2:max(k), y = rep(0, max(k) - 1), col = "grey")
dev.off()

plot(residuals(model))

####### An ARMA(1,1) process:

max_t <- 50
Z <- rnorm(max_t, 0, 1)

X <- as.vector(rep(NA, max_t))
mu <- 2
a1 <- 0.5
b1 <- 0.5
X[1] <- mu
for(i in 2:max_t) X[i] <- a1*(X[i-1] - mu) + Z[i] + b1*Z[i-1] + mu

t <- 1:max_t

model <- arima(X, order = c(1, 0, 1))
print(model)
mu_est <- model$coef[3]

pdf(file = "Plots/ARMA11_sample.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(X ~ t, type = "l", ylab = "X_t")
points(X ~ t)
abline(h = mu_est, lty = 2)
abline(h = mu, lty = 2, col = "red")
dev.off()

k <- 0:20

pdf(file = "Plots/ARMA11_sample_acf.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(X, lag.max = 20, main = "") # sample ac.f
dev.off()

plot(residuals(model))

####### An ARIMA(1,1,1) process:

max_t <- 50
Z <- rnorm(max_t, 0, 1)

t <- 1:max_t
X <- as.vector(rep(NA, max_t))
X[1] <- 0
X[2] <- 0
a1 <- 0.5
b1 <- 0.5
for(i in 3:max_t) X[i] <- (1 + a1)*X[i-1] - a1*X[i-2] + Z[i] + b1*Z[i-1]

library(dplyr)
delta_X <- lead(X, 1) - X # first differences
delta_X <- delta_X[which(!is.na(delta_X))]

pdf(file = "Plots/ARIMA_sample.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(X ~ t, type = "l", ylab = "X_t")
points(X ~ t)
dev.off()

pdf(file = "Plots/ARIMA_sample_acf.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(X, lag.max = 20, main = "") # sample ac.f
dev.off()

pdf(file = "Plots/ARIMA_sample_diffs.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(delta_X, type = "l", ylab = "delta X_t")
points(delta_X)
dev.off()

pdf(file = "Plots/ARIMA_sample_acf_diffs.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(delta_X, lag.max = 20, main = "") # sample ac.f
dev.off()

model <- arima(X, order = c(1, 1, 1))
print(model)

model2 <- arima(delta_X, order = c(1, 0, 1))
print(model2)

plot(residuals(model))


  