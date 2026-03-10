####### A SARIMA(1,0,0)x(0,1,1)_12 process:

max_t <- 1000
Z <- rnorm(max_t, 0, 1)

t <- 1:max_t
X <- as.vector(rep(NA, max_t))
for(i in 1:13) X[i] <- 0
alpha <- 0.5
Theta <- 0.5
for(i in 14:max_t) X[i] <- X[i-12] + alpha*(X[i-1] - X[i-13]) + Z[i] + Theta*Z[i-12]

pdf(file = "Plots/SARIMA_sample.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
Y <- X[601:1000]
plot(Y, type = "l", ylab = "X_t", xlab = "t")
#points(X ~ t)
dev.off()

pdf(file = "Plots/SARIMA_sample_acf.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
acf(Y, lag.max = 36, main = "")
dev.off()

library(dplyr)
delta_Y <- lead(Y, 12) - Y # seasonal differences
delta_Y <- delta_Y[which(!is.na(delta_Y))]

pdf(file = "Plots/SARIMA_delta_12.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
plot(delta_Y, type = "l", ylab = expression(paste(nabla[12]*X[t])), xlab = "t")
dev.off()

pdf(file = "Plots/SARIMA_delta_12_sample_acf.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
acf(delta_Y, lag.max = 36, main = "")
dev.off()

####### A SARIMA(1,1,0)x(0,1,1)_12 process:

max_t <- 1000
Z <- rnorm(max_t, 0, 1)

t <- 1:max_t
X <- as.vector(rep(NA, max_t))
for(i in 1:14) X[i] <- 0
alpha <- 0.5
Theta <- 0.5
for(i in 15:max_t) X[i] <- (1+alpha)*(X[i-1] - X[i-13]) - alpha*(X[i-2] - X[i-14]) + X[i-12] + Z[i] + Theta*Z[i-12]

pdf(file = "Plots/SARIMA2_sample.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
Y <- X[801:1000]
Y <- Y - Y[1]
plot(Y, type = "l", ylab = "X_t", xlab = "t")
dev.off()

pdf(file = "Plots/SARIMA2_sample_acf.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
acf(Y, lag.max = 25, main = "")
dev.off()

delta_Y <- lead(Y, 12) - Y # seasonal differences
delta_Y <- delta_Y[which(!is.na(delta_Y))]

pdf(file = "Plots/SARIMA2_sample_delta_Y.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
plot(delta_Y, type = "l", ylab = expression(paste(nabla[12]*X[t])), xlab = "t")
dev.off()

pdf(file = "Plots/SARIMA2_sample_delta_Y_acf.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
acf(delta_Y, lag.max = 25, main = "")
dev.off()

delta2_Y <- lead(delta_Y, 1) - delta_Y # first differences
delta2_Y <- delta2_Y[which(!is.na(delta2_Y))]

pdf(file = "Plots/SARIMA2_sample_delta_delta_12_Y.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
plot(delta2_Y, type = "l", ylab = expression(paste(nabla*nabla[12]*X[t])), xlab = "t")
dev.off()

pdf(file = "Plots/SARIMA2_sample_delta_delta_12_Y_acf.pdf", width = 6, height = 3) 
par(mar = c(4,4,1,1))
acf(delta2_Y, lag.max = 25, main = "")
dev.off()

####### Fitting a SARIMA(1,1,0)x(0,1,1)_12 model to the above time series:

model <- arima(X, order = c(1, 1, 0), 
               seasonal = list(order = c(0, 1, 1), period = 12))
print(model)

plot(residuals(model))

acf(residuals(model))

####### An AR(3) process:

max_t <- 1e3
Z <- rnorm(max_t, 0, 1)

X <- as.vector(rep(NA, max_t))
X[1] <- 0
X[2] <- 0
X[3] <- 0
a1 <- 1/4
a2 <- -1/2
a3 <- -1/4
for(i in 4:max_t) X[i] <- a1*X[i-1] + a2*X[i-2] + a3*X[i-3] + Z[i]

pdf(file = "Plots/AR3_pacf_alt.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
pacf(X, lag.max = 20, main = "") # sample partial ac.f
points(1, a1, col = "red")
dev.off()

