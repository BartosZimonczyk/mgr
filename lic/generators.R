ranydist <- function(n, a, b, f, M, ...){
  Y <- c()
  i <- 1
  M <- M*(b-a)
  while(length(Y) < n){
    x <- runif(1, a, b)
    u <- runif(1, 0, 1)
    if(u < f(x, ...)/M){
      Y[i] <- x
      i <- i + 1
    }
  }
  return(Y)
}

EV <- function(x, theta){exp(x-theta - exp(x-theta))}
new_beta <- function(x, theta){0.3*(dbeta(x - 1, 1, 2) + dbeta(x, 2, 1)) + 0.4*dbeta(x-0.5, 1, theta)}
mix_beta <- function(x, theta){0.1*(dbeta(x-1,1,2) + dbeta(x,2,1)) + 0.8*dbeta(x+2^(-1/theta) - 1, 1,theta)}
new_sin <- function(x, theta, j){0.5 + theta*sin(pi * j *x)}
normal_fencher <- function(x, dzeta){dnorm(x/(1+dzeta))*(x <= 0) + dnorm(x/(1-dzeta))*(x > 0)}
cauchy_fencher <- function(x, dzeta){dcauchy(x/(1+dzeta))*(x <= 0) + dcauchy(x/(1-dzeta))*(x > 0)}
lehm <- function(x, theta){(theta * 0.5^theta * (x+1)^(theta-1))*(abs(x) <= 1)}
N2B2 <- function(x, theta){0.25*(dnorm(x-2) + dnorm(x+2)) + dbeta(4*x+4, theta, 4) + 0.75*dbeta(3*x, 6, 3)}
LC <- function(x, theta){0.7*dnorm(x-theta/0.7) + 0.3*dnorm(x+theta/0.3)}
NC2 <- function(x, theta){0.3*dnorm(x) + 0.4*dcauchy(x-theta) + 0.3*dcauchy(x+2*theta)}
Chi2 <- function(x, theta){(dchisq(-x, theta) + dchisq(x, 6))/2}
NC <- function(x, theta){0.5*dnorm(x) + 0.5*dcauchy(x-theta)}
B3 <- function(x, theta){
  m <- (2/3)/(1+theta-2/3)
  0.1*dbeta(x+1, 1, 2) + 0.1*dbeta(x, 2, 1) + 0.8*dbeta(x+m, 1, theta)
}
rTuk <- function(n, t1, t2){
  U <- runif(n, 0, 1)
  return((U^t1 - 1)/t1 - ((1-U)^t2 - 1)/t2)
}

make_hist <- function(f, a, b, ...){
  p <- seq(a, b, 0.01)
  X <- ranydist(1000, a, b, f, max(f(p, ...)), ...)
  m <- median(X)
  hist(X, freq = F)
  lines(p, f(p, ...), col = 'red')
  lines(c(m,m), c(0,1), col = 'red', lwd = 3)
}

make_hist(EV, -5, 5, 2)
make_hist(LC, -10, 10, 2)
make_hist(new_beta, 0, 2, 2)
make_hist(new_sin, -1, 1, 0.35, 1.5)
make_hist(mix_beta, 0, 2, 1.2)
make_hist(N2B2, -5, 5, 0.5)
make_hist(Chi2, -20, 20, 2)
make_hist(NC, -20, 20, 3.4)
make_hist(B3, -5, 5, 1)
make_hist(lehm, -1, 1, 1.6)

X <- rTuk(1000, 7, 1.8)
m <- median(X)
hist(X, freq = F)
lines(c(m,m), c(0,1), col = 'red', lwd = 3)


NS <- c(rep(57.9, 5), rep(52.4, 4), rep(62.6, 4), rep(72.4, 6))
NL <- c(rep(53.8, 5), rep(56.7, 4), rep(65.4, 4), rep(67.1, 6))
TS <- c(rep(66.7, 5), rep(53.4, 4), rep(66.4, 4), rep(61.1, 6))
TL <- c(rep(58.8, 5), rep(55.6, 4), rep(64.9, 4), rep(61.6, 6))
V <- c(rep(64.5, 5), rep(5.7, 4), rep(41.3, 4), rep(73.8, 6))
ST <- c(rep(15.8, 5), rep(14.9, 4), rep(30, 4), rep(56.6, 6))
W <- c(rep(1.9, 5), rep(7.4, 4), rep(20.1, 4), rep(78, 6))
MG <- c(rep(69.8, 5), rep(18.4, 4), rep(68.7, 4), rep(63, 6))

mean(NS)
mean(NL)
mean(TS)
mean(TL)
mean(V)
mean(ST)
mean(W)
mean(MG)


