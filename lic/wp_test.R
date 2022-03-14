Zs_test <- function(X){
  n <- length(X)
  S <- sum(X > 0)
  Zs <- (S - n/2)/sqrt(n/4)
  return(Zs)
}

Zw_test <- function(X, p = 0.8){
  n <- length(X)
  X_sort <- sort(X)
  V <- X_sort > 0
  wp <- 0
  for(k in ceiling(n*p+1):n){
    wp <- wp + (k - n*p)*V[k]
  }
  Ewp <- 0.5*n*(1-p)*(n*(1-p)+1)
  Varwp <- m*n/12/(n-1) * (1-p)*(n*(1-p)+1)*(n*(1-p)*(3*p+1)+3*p-1)
  Zw <- (wp - Ewp)/sqrt(Varwp)
  return(Zw)
}

Zs_crit <- qnorm(1-0.01/2)
Zw_crit <- qnorm(1-0.0404/2)


