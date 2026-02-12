# Exercise 3.1:

max_t <- 102
Z <- rnorm(max_t, 0, 1)
X <- Z + 0.7*lag(Z, 1) - 0.2*lag(Z, 2)
t <- (0:(max_t-1))-2

plot(Z ~ t, type = "l", col = "grey", xlim = c(0, max_t), ylim = c(-4, 4), xlab = "t", ylab = "X_t")
points(X ~ t)
lines(X ~ t)
abline(h = 0, lty = 2)

acf(X[3:max_t], main = "")
points(0, 1, lty = 2, col = "red")
points(1, 0.56/1.53, lty = 2, col = "red")
points(2, -0.2/1.53, lty = 2, col = "red")



