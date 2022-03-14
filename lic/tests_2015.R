library(polynom)
library(orthopolynom)

wielomiany <- slegendre.polynomials(30, normalized = T)
bs <- sapply(wielomiany, as.function)

##################
# Tk test

T_test <- function(X, dn, type, Dn=2, deltan=0.03){
  n <- length(X)
  
  if(type == 'S'){
    k <- S_choice_T(X, dn)
  }
  
  if(type == 'L'){
    k <- L_choice_T(X, dn, Dn, deltan)
  }
  
  if(type != 'L' && type != 'S'){
    return('error')
  }
  
  g <- g_array(dn)
  ranks <- rank(c(X, -X))
  g_hat <- c()
  #cat('main k', k, '\n')
  for(j in 1:k){
    
    inside_sum <- 0
    for(i in 1:n){
      inside_sum <- inside_sum + g[[j]]((2*ranks[i] - 1)/(4*n))
    }
    g_hat[j] <- 1/sqrt(n) * inside_sum
  }
    
  return(data.frame('stat'=sum(g_hat^2), 'k'=k))
}

#test T pomocniczy w wyliczaniu S i L
T_test_0 <- function(k, X, g){
  n <- length(X)
  
  g_hat <- c()
  ranks <- rank(c(X, -X))
  #cat('0k:', k, '\n')
  for(j in 1:k){
    
    inside_sum <- 0
    for(i in 1:n){
      inside_sum <- inside_sum + g[[j]]((2*ranks[i] - 1)/(4*n))
    }
    g_hat[j] <- 1/sqrt(n) * inside_sum
  }
  
  return(sum(g_hat^2))
}


##################
# wybór S

T_test_red_for_S <- function(k, X, g){
  return(T_test_0(k, X, g) - k * log(length(X)))
}

S_choice_T <- function(X, dn){
  n <- length(X)
  g <- g_array(dn)
  
  Tks <- sapply(1:dn, T_test_red_for_S, X, g)
  S <- which(Tks == max(Tks))
  return(min(S))
}

###################

#wyliczanie pułapu cjn
cj_treshold_T <- function(dn, Dn, deltan){
  cj <- c()
  index <- 1
  
  for(i in 1:Dn){
    ds <- deltan/(Dn*choose(dn, i))
    inside <- 1 - 0.5*ds^(1/i)
    cj[index] <- qnorm(inside)
    index <- index + 1
  }
  #cat(cj, '\n')
  return(cj)
}

# funkcja kary pi

pi_function_T <- function(k, X, g, dn, Dn, deltan, cj){
  n <- length(X)
  #cat(n, '\n')
  g_hat <- c()
  ranks <- rank(c(X, -X))
  for(j in 1:dn){
    
    inside_sum <- 0
    for(i in 1:n){
      inside_sum <- inside_sum + g[[j]]((2*ranks[i] - 1)/(4*n))
    }
    g_hat[j] <- (1/sqrt(n) * inside_sum)^2
  }
  
  g_hat_sorted <- sort(g_hat)
  g_hat_new <- g_hat_sorted[dn:(dn - Dn + 1)]
  
  W <- ifelse(any(g_hat_new >= cj^2), 2*k, k*log(n))

  return(W)
}

#wybór L

T_test_red_for_L <- function(k, X, g, dn, Dn, deltan, cj){
  ttest <- T_test_0(k, X, g)
  pifun <- pi_function_T(k, X, g, dn, Dn, deltan, cj)
  return(ttest - pifun)
}

L_choice_T <- function(X, dn, Dn, deltan){
  n <- length(X)
  g <- g_array(dn)
  
  cj <- cj_treshold_T(dn, Dn, deltan)
  
  Tks <- sapply(1:dn, T_test_red_for_L, X, g, dn, Dn, deltan, cj)
  
  #cat(paste(Tks), '\n')
  
  L <- which(Tks == max(Tks))
  return(min(L))
}

######################
# tworzenie ciągu funkcyjnego g

g_array <- function(dn){
  lim <- 4
  lim <- ifelse(dn == 4, 2, lim)
  lim <- ifelse(dn == 5, 3, lim)
  lim <- ifelse(dn == 6, 4, lim)
  
  delta <- 3/(8*dn)
  
  #tworzenie ciągu funkcji b, używane będą tylko b2, b4 itd któr odpowiadają
  #nieparzystem wielomianom Legendre'a, gdyż b1 to wielomian stopnia 0
  b_red <- list()
  b_red_sq <- list()
  
  for(j in 1:lim){
    b_red[[j]] <- function(t){(t >= delta && t <= 0.5-delta)*bs[[2*j]]((t-delta)*0.5/(0.5 - 2*delta))}
    b_red_sq[[j]] <- function(t){b_red[[j]](t)^2}
  }

  g <- list()
  for(j in 1:lim){
    g[[j]] <- function(t){
      const <- integrate(b_red_sq[[j]], delta, 0.5-delta)$value
      if(t > 0.5){
        return(-b_red[[j]](1-t)/sqrt(2*const))
      }else{
        return(b_red[[j]](t)/sqrt(2*const))
      }
      
    }
  }

  #funkcja h_c
  hc <- function(t){sign(2*t - 1)*(2*delta)^(-1/2)*(abs(2*t - 1) >= 0 && abs(2*t - 1) <= 2*delta)}
  
  #funkcja h_I
  hI <- function(t, u, v){
    C <- sign(2*t - 1)*sqrt(3/(62*(v-u)^3))
    
    ind <- ((1 + abs(2*t - 1)) >= 2*u) && ((1 + abs(2*t - 1)) <= 2*v)
    
    return(C*(abs(4*t - 2) + 2 + v - 5*u)*ind)
  }
  
  #jako, że wykorzystana bedzie tylko funkcja h_1 można ją stworzyć odręcznie
  h1 <- function(t){hI(t, 1-delta, 1)}
  
  if(dn == 4){
    return(list(g[[2]], h1, g[[1]], hc))
  }
  
  if(dn == 5){
    return(list(g[[2]], h1, g[[1]], hc, g[[3]]))
  }
  
  if(dn == 6){
    return(list(g[[2]], h1, g[[1]], hc, g[[3]], g[[4]]))
  }
}

TSS <- c()
TLL <- c()
XD <- 100
TS <- matrix(nrow = XD, ncol = 2)
TL <- matrix(nrow = XD, ncol = 2)
MC <- list(c(), c(), c(), c(), c(), c(), c())

  for(i in 1:XD){
    X1 <- runif(50) - 1/2
    X2 <- runif(100) - 1/2
    TS[i,] <- c(T_test(X1, 5, 'S')$stat, T_test(X2, 6, 'S')$stat)
    TL[i,] <- c(T_test(X1, 5, 'L', 2, 0.02)$stat, T_test(X1, 6, 'L', 3, 0.05)$stat)
    cat(i, '\n')
    # MC[[1]][i] <- NS_test(X, 12)
    # MC[[2]][i] <- NL_test(X, 12, 1, 0.01)
    # MC[[3]][i] <- NL_test(X, 12, 3, 0.01)
    # MC[[4]][i] <- NL_test(X, 12, 1, 0.03)
    # MC[[5]][i] <- NL_test(X, 12, 3, 0.03)
    # MC[[6]][i] <- NL_test(X, 12, 1, 0.05)
    # MC[[7]][i] <- NL_test(X, 12, 3, 0.05)
  }
  
  df <- data.frame('n = 50' = c(quantile(TS[,1], probs = 0.95), quantile(TL[,1], probs = 0.95)),
                   'n = 100' = c(quantile(TS[,2], probs = 0.95), quantile(TL[,2], probs = 0.95)),
                   row.names = c('TS', 'TL'))
  
  df
  
  write.csv(df, file = 'T_quantiles.csv')
  
  # quantile(MC[[1]], probs = 0.95)
  # quantile(MC[[2]], probs = 0.95)
  # quantile(MC[[3]], probs = 0.95) 
  # quantile(MC[[4]], probs = 0.95)
  # quantile(MC[[5]], probs = 0.95)
  # quantile(MC[[6]], probs = 0.95)
  # quantile(MC[[7]], probs = 0.95)
  
