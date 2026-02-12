max_t <- 1002
Z <- rnorm(max_t, 0, 1)
beta <- 5

X <- Z + beta*lag(Z, 1)
t <- (0:(max_t-1))-2

pdf(file = "Plots/MA1_beta5.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(X ~ t, type = "l", xlim = c(0, 100), ylim = c(-20, 20), xlab = "t", ylab = "X_t")
points(X ~ t)
abline(h = 0, lty = 2)
dev.off()

pdf(file = "Plots/MA1_beta5_acf.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(X[3:max_t], main = "")
points(0, 1, lty = 2, col = "red")
points(1, beta/(1+beta^2), lty = 2, col = "red")
dev.off()

####

Z_dash <- 5*Z
Y <- Z_dash + 1/beta*lag(Z_dash, 1)
t <- (0:(max_t-1))-2

pdf(file = "Plots/MA1_beta1over5.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(Y ~ t, type = "l", xlim = c(0, 100), ylim = c(-20, 20), xlab = "t", ylab = "X_t")
points(Y ~ t)
lines(Y ~ t)
abline(h = 0, lty = 2)
dev.off()

pdf(file = "Plots/MA1_beta1over5_acf.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(Y[3:max_t], main = "")
points(0, 1, lty = 2, col = "red")
points(1, beta/(1+beta^2), lty = 2, col = "red")
dev.off()

#### combined:

pdf(file = "Plots/MA1_both.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(X ~ t, type = "l", xlim = c(0, 50), ylim = c(-15, 15), xlab = "t", ylab = "X_t", col = "blue")
points(X ~ t, col = "blue")
lines(Y ~ t, col = "red")
points(Y ~ t, col = "red")
#lines(5 * Z ~ t, col = "grey")
#lines(5 * lag(Z, 1) ~ t, col = "grey")
abline(h = 0, lty = 2)
dev.off()

plot(X - lag(Y, 1) ~ t, type = "l", xlim = c(0, 50))
var(X - lag(Y, 1), na.rm = TRUE)
var(X, na.rm = TRUE)



