N2B2 <- function(x, theta){0.25*(dnorm(x-2) + dnorm(x+2)) + dbeta(4*x+4, theta, 4) + 0.75*dbeta(3*x, 6, 3)}

x <- seq(-1, 1, 0.01)
N2B2_dist <- N2B2(x, 12)
plot(x, N2B2_dist, type="l")
inputs <- (1:201 - 0.5) / 201
ind <- which(inputs < 0.30 | inputs > 0.70)
lines(x[ind], N2B2_dist[ind], type="l", col="red")