mu <- 0.2

Z <- rnorm(50, mu, 1)
X <- c(0, cumsum(Z))
t <- 0:50

sd <- sqrt(t)
mu_X <- mu * t

pdf(file = "Plots/random_walk.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))

plot(X ~ t, type = "l", ylim = c(-10, 30), col = "grey90", ylab = "X_t")

for(i in 1:20) {
  Z <- rnorm(50, mu, 1)
  X <- c(0, cumsum(Z))
  lines(X ~ t, col = "grey90")
}

lines(X ~ t, col = "blue")

lines(2*sd + mu_X ~ t, lty =2, col = "red")
lines(-2*sd + mu_X ~ t, lty =2, col = "red")
lines(mu_X ~ t, lty = 2)

dev.off()

pdf(file = "Plots/random_walk_differences.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
plot(Z ~ t, type = "l", ylim = c(-10, 30), col = "grey90", ylab = "X_t - X_{t-1}")
dev.off()

#########

max_t <- 104
Z <- rnorm(max_t, 0, 1)
X <- stats::filter(Z, filter = rep(1/5, 5), sides = 1)
t <- (0:(max_t-1))-4

pdf(file = "Plots/moving_average.pdf", width = 6, height = 3.2) 
par(mar = c(4,4,1,1))
plot(Z ~ t, type = "l", col = "grey", xlim = c(0, max_t), ylim = c(-2, 2), xlab = "t", ylab = "X_t")
points(X ~ t)
lines(X ~ t)
abline(h = 2*sqrt(5*(1/5)^2), lty =2, col = "red")
abline(h = -2*sqrt(5*(1/5)^2), lty =2, col = "red")
abline(h = 0, lty = 2)
dev.off()

pdf(file = "Plots/moving_average_acf.pdf", width = 4, height = 3.2) 
par(mar = c(4,4,1,1))
acf(X[5:max_t], main = "")
t_short <- 0:20
points(pmax(1 - t_short/5, 0) ~ t_short, lty = 2, col = "red")
dev.off()



