library(polynom)
library(orthopolynom)

################
# Tk test
wielomiany <- slegendre.polynomials(30, normalized = T)
b <- sapply(wielomiany, as.function)
ks <- c()

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
  
  return(c(sum(g_all[1:k]), k))
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
  delta <- 3/(8*dn)
  
  #tworzenie ciągu funkcji b, używane będą tylko b2, b4 itd któr odpowiadają
  #nieparzystem wielomianom Legendre'a, gdyż b1 to wielomian stopnia 0
  b_red <- list()
  b_red_sq <- list()
  
  b_red[[1]] <- function(t){(t >= delta && t <= 0.5-delta)*b[[2*1]]((t-delta)*0.5/(0.5 - 2*delta))}
  b_red_sq[[1]] <- function(t){b_red[[1]](t)^2}
  
  b_red[[2]] <- function(t){(t >= delta && t <= 0.5-delta)*b[[2*2]]((t-delta)*0.5/(0.5 - 2*delta))}
  b_red_sq[[2]] <- function(t){b_red[[2]](t)^2}
  
  b_red[[3]] <- function(t){(t >= delta && t <= 0.5-delta)*b[[2*3]]((t-delta)*0.5/(0.5 - 2*delta))}
  b_red_sq[[3]] <- function(t){b_red[[3]](t)^2}
  
  b_red[[4]] <- function(t){(t >= delta && t <= 0.5-delta)*b[[2*4]]((t-delta)*0.5/(0.5 - 2*delta))}
  b_red_sq[[4]] <- function(t){b_red[[4]](t)^2}
  
  g <- list()
  
  const <- integrate(b_red_sq[[1]], delta, 0.5-delta)$value
  g[[1]] <- function(t){ifelse(t > 0.5, -b_red[[1]](1-t)/sqrt(2*const), b_red[[1]](t)/sqrt(2*const))}
  
  const <- integrate(b_red_sq[[2]], delta, 0.5-delta)$value
  g[[2]] <- function(t){ifelse(t > 0.5, -b_red[[2]](1-t)/sqrt(2*const), b_red[[2]](t)/sqrt(2*const))}
  
  const <- integrate(b_red_sq[[3]], delta, 0.5-delta)$value
  g[[3]] <- function(t){ifelse(t > 0.5, -b_red[[3]](1-t)/sqrt(2*const), b_red[[3]](t)/sqrt(2*const))}
  
  const <- integrate(b_red_sq[[4]], delta, 0.5-delta)$value
  g[[4]] <- function(t){ifelse(t > 0.5, -b_red[[4]](1-t)/sqrt(2*const), b_red[[4]](t)/sqrt(2*const))}
  
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

################
# Nk test

N_test <- function(X, dn = 1, Dn = 1, deltan = 0.01, type = 'S'){
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

###############
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

##########
# Wp test

Z_test <- function(X){
  sum(sign(X))
}

Wp_test <- function(X, p=0.8){
  ranks <- rank(abs(X))
  N <- length(X)
  Wp <- 0
  for(i in 1:N){
    inside <- max(c(0, ranks[i] - N*p))*(X[i] >= 0)
    Wp <- Wp + inside
  }
  n <- sum(X > 0)
  m <- N - n
  Ewp <- 0.5*n*(1-p)*(N*(1-p)+1)
  Varwp <- m*n/12/(N-1) * (1-p)*(N*(1-p)+1)*(N*(1-p)*(3*p+1)+3*p-1)
  Wp <- (Wp - Ewp)/sqrt(Varwp)
  return(Wp)
}

#########
# test Studenta
T_st_test <- function(X){
  sqrt(length(X))*mean(X)/sd(X)
}


###########
# generowanie alternatyw

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

#########
# liczenie mocy

NS_crit <- 5.443637
NL_crit <- 6.615899
TS_crit <- 5.397339
TL_crit <- 7.183035
V_crit <- 4.020356
WC_crit <- 3.763536
Z_crit <- qnorm(1-0.01/2)
WP_crit <- qnorm(1-0.0404/2)
T_crit <- qt(1-0.05/2, 99)

left <- -1
right <- 1
p <- seq(left, right, 0.01)
theta <- 1.2
NS <- c()
NL <- c()
TS <- c()
TL <- c()
V <- c()
WC <- c()
Z <- c()
WP <- c()
tSt <- c()
set.seed(73)
for(i in 1:10000){
  X <- ranydist(100, left, right, lehm, max(lehm(p, theta)), theta)
  X <- X - median(X)
  NS[i] <- N_test(X, dn = 10, type = 'S')
  NL[i] <- N_test(X, dn = 10, Dn = 3, deltan = 0.05, type = 'L')
  TS[i] <- T_test(X, dn = 6, type = 'S')[1]
  TL[i] <- T_test(X, dn = 6, Dn = 3, deltan = 0.05, type = 'L')[1]
  V[i] <- log(V_test(X))
  WC[i] <- N_test(X, type = 'W')
  Z[i] <- Z_test(X)
  WP[i] <- Wp_test(X)
  tSt[i] <- abs(T_st_test(X))
}

MG <- (abs(Z) > Z_crit) | (abs(Z) <= Z_crit & abs(WP) > WP_crit)

df <- data.frame('NS' = round(mean(NS > NS_crit), 3)*100,
                 'NL' = round(mean(NL > NL_crit), 3)*100,
                 'TS' = round(mean(TS > TS_crit), 3)*100,
                 'TL' = round(mean(TL > TL_crit), 3)*100,
                 'V' = round(mean(V > V_crit), 3)*100,
                 'ST' = round(mean(tSt > T_crit), 3)*100,
                 'WC' = round(mean(WC > WC_crit), 3)*100,
                 'MG' = round(mean(MG), 3)*100)

df


