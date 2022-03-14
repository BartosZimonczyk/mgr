library(polynom)
library(orthopolynom)


################
# Nk test
wielomiany <- slegendre.polynomials(24, normalized = T)
b <- sapply(wielomiany, as.function)


N_test <- function(X, dn = 2, Dn = 1, deltan = 0.01, type = 'S'){
  n <- length(X)
  k <- 1
  b_all <- b_hat_sq(dn, X)
  
  if(type == 'S'){
    Nks <- sapply(1:dn, function(y) sum(b_all[1:y]) - y*log(n))
    k <- min(which(Nks == max(Nks)))
  }
  
  if(type == 'L'){
    sorted_bs <- sort(b_all)[dn:(dn-Dn+1)]
    cjs <- cj_treshold(1:Dn, dn, Dn, deltan)
    W <- any(sorted_bs >= cjs^2)
    Nks <- sapply(1:dn, function(y) sum(b_all[1:y]) - (2*y*W + y*log(n)*(1-W)))
    k <- min(which(Nks == max(Nks)))
  }
  
  if(type == 'W'){
    k <- 1
  }
  
  return(sum(b_all[1:k]))
}

cj_treshold <- function(j, dn, Dn, deltan){
  inside <- 1 - 0.5 * (deltan/Dn/choose(dn, j))^(1/j)
  return(qnorm(inside))
}

b_hat_sq <- function(k, X){
  n <- length(X)
  
  b_hat_sq <- c()
  ranks <- rank(c(X, -X))
  for(j in 1:k){
    inside <- 0
    for(i in 1:n){
      inside <- inside + b[[j*2]]((ranks[i] - 0.5)/(2*n))
    }
    b_hat_sq[j] <- 1/n * inside^2
  }
  
  return(b_hat_sq)
}


W1 <- c()
W2 <- c()
W3 <- c()
set.seed(42)
for(i in 1:10000){
  X1 <- rnorm(25)
  X2 <- rnorm(50)
  X3 <- rnorm(100)
  
  W1[i] <- N_test(X1, type = 'W')
  W2[i] <- N_test(X2, type = 'W')
  W3[i] <- N_test(X3, type = 'W')
  cat(i, '\n')
}

df <- data.frame('n25' = quantile(W1, probs = c(0.95, 0.9)),
                 'n50' = quantile(W2, probs = c(0.95, 0.9)),
                 'n100' = quantile(W3, probs = c(0.95, 0.9)))

df










