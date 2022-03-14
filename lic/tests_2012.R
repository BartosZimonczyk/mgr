library(polynom)
library(orthopolynom)


################
# Nk test
wielomiany <- slegendre.polynomials(30, normalized = T)
b <- sapply(wielomiany, as.function)

N_test <- function(k, X){
  n <- length(X)
  b_hat <- c()
  ranks <- rank(c(X, -X))
  
  for(j in 1:k){
    inside_sum <- 0
    for(i in 1:n){
      inside_sum <- inside_sum + b[[2*j]]((ranks[i] - 0.5)/(2*n))
    }
    b_hat[j] <- 1/sqrt(n) * inside_sum
  }
  return(sum(b_hat^2))
}

NS_test <- function(X, dn){
  return(N_test(S_choice_N(X, dn), X))
}

NL_test <- function(X, dn, Dn, deltan){
  return(N_test(L_choice_N(X, dn, Dn, deltan), X))
}

################
# wybór S

N_test_red_for_S <- function(k, X){
  return(N_test(k, X) - k * log(length(X)))
}

S_choice_N <- function(X, dn){
  n <- length(X)
  
  Nks <- sapply(1:dn, N_test_red_for_S, X)
  S <- which(Nks == max(Nks))
  #cat('NKS:', paste(Nks), '\n')
  #cat('S:', min(S), '\n')
  return(min(S))
}


################

#wyliczanie b z daszkiem
emp_Fourier_coefs <- function(k, X){
  n <- length(X)
  ranks <- rank(c(X, -X))
  b_emp <- c()
  index <- 1
  for(j in 1:k){
    inside_sum <- 0
    
    for(i in 1:n){
      R <- (ranks[i] - 0.5)/(2*n)
      v <- b[[2*j]](R)
      inside_sum <- inside_sum + v
    }
    
    b_emp[index] <- 1/sqrt(n)*inside_sum
    index <- index + 1
  }
  return(b_emp)
}


#wyliaczanie pułapu 
cj_treshold <- function(dn, Dn, deltan){
  cj <- c()
  
  for(i in 1:3){
    ds <- deltan/Dn/choose(dn, i)
    inside <- 1 - 0.5*ds^(1/i)
    cj[i] <- qnorm(inside)
  }
  return(cj[1:max(c(2, Dn))]^2)
}

#funkcja kary pi
pi_function <- function(k, X, dn, Dn, deltan, cj){
  n <- length(X)
  b_emp <- emp_Fourier_coefs(dn, X)
  b_sorted <- sort(b_emp^2)
  
  b_new <- b_sorted[dn:min(c(dn-Dn+1, dn-1))]
  #cat('cj: ', length(cj), ', b: ', length(b_new), '\n')
  W <- ifelse(any(b_new >= cj), 2*k, k*log(n))
  return(W)
}

# wybór L
N_test_red_for_L <- function(k, X, dn, Dn, deltan, cj){
  return(N_test(k, X) - pi_function(k, X, dn, Dn, deltan, cj))
}

L_choice_N <- function(X, dn, Dn, deltan){
  n <- length(X)
  
  cj <- cj_treshold(dn, Dn, deltan)
  
  Nks <- sapply(1:dn, N_test_red_for_L, X, dn, Dn, deltan, cj)
    
  L <- which(Nks == max(Nks))
  #cat('NKS:', paste(Nks), '\n')
  #cat('L:', min(L), '\n')
  return(min(L))
}


################
# małe testy


MC1 <- list(c(), c(), c(), c(), c(), c(), c())
MC2 <- list(c(), c(), c(), c(), c(), c(), c())
for(i in 1:1000){
  X1 <- runif(50) - 0.5
  MC1[[1]][i] <- NS_test(X1, 10)
  MC1[[2]][i] <- NL_test(X1, 10, 1, 0.01)
  MC1[[3]][i] <- NL_test(X1, 10, 3, 0.01)
  MC1[[4]][i] <- NL_test(X1, 10, 1, 0.03)
  MC1[[5]][i] <- NL_test(X1, 10, 3, 0.03)
  MC1[[6]][i] <- NL_test(X1, 10, 1, 0.05)
  MC1[[7]][i] <- NL_test(X1, 10, 3, 0.05)
  #cat(i, '\n')
  X2 <- runif(100) - 0.5
  MC2[[1]][i] <- NS_test(X2, 12)
  MC2[[2]][i] <- NL_test(X2, 12, 1, 0.01)
  MC2[[3]][i] <- NL_test(X2, 12, 3, 0.01)
  MC2[[4]][i] <- NL_test(X2, 12, 1, 0.03)
  MC2[[5]][i] <- NL_test(X2, 12, 3, 0.03)
  MC2[[6]][i] <- NL_test(X2, 12, 1, 0.05)
  MC2[[7]][i] <- NL_test(X2, 12, 3, 0.05)
  cat(i, '\n')
}


df <- data.frame('n = 50' = c(quantile(MC1[[1]], probs = 0.95),
                              quantile(MC1[[2]], probs = 0.95),
                              quantile(MC1[[3]], probs = 0.95),
                              quantile(MC1[[4]], probs = 0.95),
                              quantile(MC1[[5]], probs = 0.95),
                              quantile(MC1[[6]], probs = 0.95),
                              quantile(MC1[[7]], probs = 0.95)),
                 'n = 100' = c(quantile(MC2[[1]], probs = 0.95),
                               quantile(MC2[[2]], probs = 0.95),
                               quantile(MC2[[3]], probs = 0.95),
                               quantile(MC2[[4]], probs = 0.95),
                               quantile(MC2[[5]], probs = 0.95),
                               quantile(MC2[[6]], probs = 0.95),
                               quantile(MC2[[7]], probs = 0.95)),
                 row.names = c('NS', 'NL1_1', 'NL1_3', 'NL3_1', 'NL3_3', 'NL5_1', 'NL5_3'))

df

write.csv(df, file = 'N_quantiles_t.csv')

















