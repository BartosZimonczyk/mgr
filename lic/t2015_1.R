library(polynom)
library(orthopolynom)

################
# Nk test
wielomiany <- slegendre.polynomials(30, normalized = T)
b <- sapply(wielomiany, as.function)

T_test <- function(X, dn, Dn = 1, deltan = 0.01, type = 'S'){
  n <- length(X)
  k <- 1
  
  g <- g_array(dn)
  g_all <- g_hat_sq(dn, X, g)
  
  if(type == 'S'){
    Nks <- sapply(1:dn, function(y) sum(g_all[1:y]) - y*log(n))
    k <- min(which(Nks == max(Nks)))
  }
  
  if(type == 'L'){
    sorted_gs <- sort(g_all)[dn:(dn - Dn + 1)]
    cjs <- cj_treshold(1:Dn, dn, Dn, deltan)
    W <- any(sorted_gs >= cjs^2)
    Tks <- sapply(1:dn, function(y) sum(g_all[1:y]) - (2*y*W + y*log(n)*(1-W)))
    k <- min(which(Tks == max(Tks)))
  }
  
  return(sum(g_all[1:k]))
}

cj_treshold <- function(j, dn, Dn, deltan){
  inside <- 1 - 0.5 * (deltan/Dn/choose(dn, j))^(1/j)
  return(qnorm(inside))
}

g_hat_sq <- function(k, X, g){
  n <- length(X)
  
  g_hat_sq <- c()
  ranks <- rank(c(X, -X))
  for(j in 1:k){
    inside <- 0
    for(i in 1:n){
      inside <- inside + g[[j]]((ranks[i] - 0.5)/(2*n))
    }
    g_hat_sq[j] <- 1/n * inside^2
  }
  
  return(g_hat_sq)
}

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
    b_red[[j]] <- function(t){(t >= delta && t <= 0.5-delta)*b[[2*j]]((t-delta)*0.5/(0.5 - 2*delta))}
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

Ts100 <- list(c(), c())
for(i in 1:30000){
  X2 <- rnorm(100)
  
  Ts100[[1]][i] <- T_test(X2, 6, type = 'S')
  Ts100[[2]][i] <- T_test(X2, 6, Dn = 3, deltan = 0.03, type = 'L')
  #cat(i, '\n')
}

df <- data.frame('n100' = c(quantile(Ts100[[1]], probs = 0.95),
                            quantile(Ts100[[2]], probs = 0.95)),
                 row.names = c('TS', 'TL0.03'))

df















