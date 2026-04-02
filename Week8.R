#### simple exponential smoothing of series without trend:

x <- c(2, 4, 3, 2)
L <- x[1]
alpha <- 0.3

for(t in 2:4) {
  L_next <- alpha*x[t] + (1 - alpha)*L[t-1]
  L <- c(L, L_next)
}

pred <- c(NA, L)

pdf(file = "Plots/ExpSmoothing_NoTrend.pdf", width = 4, height = 3) 
par(mar = c(4,4,1,1))
plot(pred, xlim = c(1, 6), ylim = c(1, 5), pch = 4, xlab = "t", ylab = "x_t", col = "red")
lines(pred, col = "red")
points(x)
lines(x)
dev.off()

#### simple exponential smoothing of series with trend:

x <- c(2, 4, 3, 6)
L <-x[1]
# L <- (x[1] + x[2])/2
alpha <- 0.3

for(t in 2:4) {
  L_next <- alpha*x[t] + (1 - alpha)*L[t-1]
  L <- c(L, L_next)
}

pred <- c(NA, L)
pred_ahead <- c(pred, pred[5], pred[5])

#### Holt's exponential smoothing:

L2 <- x[1]
T <- 0
# T <- 1
gamma <- 0.3

for(t in 2:4) {
  L2_next <- alpha*x[t] + (1 - alpha)*(L2[t-1] + T[t-1])
  T_next <- gamma*(L2_next - L2[t-1]) + (1 - gamma)*T[t-1]
  L2 <- c(L2, L2_next)
  T <- c(T, T_next)
}

pred2 <- c(NA, L2 + T)
pred2_ahead <- c(pred2, pred2[5] + (1:2)*T[4])

pdf(file = "Plots/ExpSmoothing_Holt_WithTrend.pdf", width = 4, height = 3) 
par(mar = c(4,4,1,1))
plot(pred, xlim = c(1, 7), ylim = c(1, 9), pch = 4, xlab = "t", ylab = "x_t", col = "red")
lines(pred, col = "red")
points(pred_ahead, pch = 4, col = "red")
lines(pred_ahead, lty = 2, col = "red")
points(x)
lines(x)
points(pred2, pch = 4, col = "orange")
lines(pred2, col = "orange")
points(pred2_ahead, pch = 4, col = "orange")
lines(pred2_ahead, lty = 2, col = "orange")
dev.off()

#t <- 1:4
#lmFit = lm(x ~ t)
#abline(lmFit, lty = 3)

