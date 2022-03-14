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

Ts50 <- list(c(), c())
Ts100 <- list(c(), c())
k1 <- c()
k2 <- c()
k3 <- c()
k4 <- c()
for(i in 1:30000){
  X1 <- rnorm(50)
  X2 <- rnorm(100)
  x1 <- T_test(X1, 5, type = 'S')
  x2 <- T_test(X1, 5, Dn = 2, deltan = 0.03, type = 'L')
  x3 <- T_test(X2, 6, type = 'S')
  x4 <- T_test(X2, 6, Dn = 3, deltan = 0.05, type = 'L')
  
  Ts50[[1]][i] <- x1[1]
  Ts50[[2]][i] <- x2[1]
  k1[i] <- x1[2]
  k2[i] <- x2[2]
  
  Ts100[[1]][i] <- x3[1]
  Ts100[[2]][i] <- x4[1]
  k3[i] <- x3[2]
  k4[i] <- x4[2]
  cat(i, '\n')
}

df <- data.frame('n50' = c(quantile(Ts50[[1]], probs = 0.95),
                           quantile(Ts50[[2]], probs = 0.95)),
                 'n100' = c(quantile(Ts100[[1]], probs = 0.95),
                            quantile(Ts100[[2]], probs = 0.95)),
                 row.names = c('TS', 'TL'))

df

summary(factor(k1))
summary(factor(k2))
summary(factor(k3))
summary(factor(k4))

par(mfrow = c(3,2))
gg <- g_array(6)
p <- seq(0, 1, 0.01)
y <- c()
for(i in 1:101){
  y[i] <- gg[[1]](p[i])
}
plot(p, y, type = 'l', main = 'b3')

y <- c()
for(i in 1:101){
  y[i] <- gg[[2]](p[i])
}
plot(p, y, type = 'l', main = 'h1')

y <- c()
for(i in 1:101){
  y[i] <- gg[[3]](p[i])
}
plot(p, y, type = 'l', main = 'b1')

y <- c()
for(i in 1:101){
  y[i] <- gg[[4]](p[i])
}
plot(p, y, type = 'l', main = 'hc')

y <- c()
for(i in 1:101){
  y[i] <- gg[[5]](p[i])
}
plot(p, y, type = 'l', main = 'b5')

y <- c()
for(i in 1:101){
  y[i] <- gg[[6]](p[i])
}
plot(p, y, type = 'l', main = 'b7')
  


