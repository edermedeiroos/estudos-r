# Item a
x_bin <- 0:10

y_bin1 <- dbinom(x_bin, size = 10, prob = 0.2)
y_bin2 <- dbinom(x_bin, size = 10, prob = 0.5)
y_bin3 <- dbinom(x_bin, size = 10, prob = 0.8)

plot(x_bin, y_bin1, type = "b", col = "red", pch = 16, lwd = 2,
     main = "Distribuições Binomiais (n=10)",
     xlab = "Número de Sucessos (x)", ylab = "Probabilidade P(X=x)",
     ylim = c(0, 0.35))

lines(x_bin, y_bin2, type = "b", col = "blue", pch = 17, lwd = 2)
lines(x_bin, y_bin3, type = "b", col = "green", pch = 18, lwd = 2)

legend("topright", legend = c("p = 0.2", "p = 0.5", "p = 0.8"),
       col = c("red", "blue", "green"), pch = c(16, 17, 18), lty = 1, lwd = 2)

# Item b
x_norm <- seq(-6, 6, length = 1000)

y_norm1 <- dnorm(x_norm, mean = 0, sd = 1)
y_norm2 <- dnorm(x_norm, mean = 0, sd = 2)
y_norm3 <- dnorm(x_norm, mean = 2, sd = 1)

plot(x_norm, y_norm1, type = "l", col = "red", lwd = 2,
     main = "Distribuições Normais",
     xlab = "Valores de X", ylab = "Densidade",
     ylim = c(0, 0.45))

lines(x_norm, y_norm2, col = "blue", lwd = 2, lty = 2)
lines(x_norm, y_norm3, col = "green", lwd = 2, lty = 3)

legend("topright", legend = c("N(0,1)", "N(0,4)", "N(2,1)"),
       col = c("red", "blue", "green"), lty = c(1, 2, 3), lwd = 2)

# Item c
x_pois <- 0:20

y_pois1 <- dpois(x_pois, lambda = 2)
y_pois2 <- dpois(x_pois, lambda = 5)
y_pois3 <- dpois(x_pois, lambda = 10)

plot(x_pois, y_pois1, type = "b", col = "red", pch = 16, lwd = 2,
     main = "Distribuições de Poisson",
     xlab = "Número de Ocorrências (x)", ylab = "Probabilidade P(X=x)",
     ylim = c(0, 0.3))

lines(x_pois, y_pois2, type = "b", col = "blue", pch = 17, lwd = 2)
lines(x_pois, y_pois3, type = "b", col = "green", pch = 18, lwd = 2)

legend("topright", legend = c("lambda = 2", "lambda = 5", "lambda = 10"),
       col = c("red", "blue", "green"), pch = c(16, 17, 18), lty = 1, lwd = 2)

