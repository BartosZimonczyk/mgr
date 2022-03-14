

# test V
V_test <- function(Z, delta=0.1){
  n <- length(Z)
  
  a <- ceiling(n^(0.5+delta))
  b <- floor(min(c(n^(1-delta), n/2)))
  
  r <- c()
  index <- 1
  for(m in a:b){
    value <- 1
    for(j in 1:n){
      value <- value * 2*m*(1-(m+1)/(2*n))/(n*Delta(Z, j, m))
    }
    r[index] <- value
    index <- index + 1
  }
  
  return(min(r))
}

#wyliacznie Delty dla testu V

Delta <- function(Z, j, m){
  n <- length(Z)
  d <- 0
  sorted_Z <- sort(Z)
  for(i in 1:n){
    a1 <- Z[i] <= sorted_Z[min(c(n, j+m))]
    a2 <- -Z[i] <= sorted_Z[min(c(n, j+m))]
    a3 <- Z[i] <= sorted_Z[max(c(1, j-m))]
    a4 <- -Z[i] <= sorted_Z[max(c(1, j-m))]
    d <- d + a1 + a2 - a3 - a4
  }
  if(d/2/n == 0){
    return(1/n)
  }else{
    return(d/2/n)
  }
}

MC1 <- c()
MC2 <- c()
set.seed(42)
for(i in 1:10000){
  Z1 <- runif(50) - 1/2
  Z2 <- runif(100) - 1/2
  MC1[i] <- log(V_test(Z1))
  MC2[i] <- log(V_test(Z2))
  cat(i, '\n')
}

df <- data.frame('n = 50' = quantile(MC1, probs = 1-c(0.05, 0.1)),
                 'n = 100' = quantile(MC2, probs = 1-c(0.05, 0.1)),
                 row.names = c('0.05', '0.1'))

df


