source("src/basic_functions.R")

cj_treshold <- function(j, dn, Dn, deltan){
  inside <- 1 - 0.5 * (deltan/Dn/choose(dn, j))^(1/j)
  qnorm(inside)
}

I_func <- function(dn, X, c_t){
    n <- length(X)
    Ls <- sapply(1:dn, L, X)
    max(abs(Ls)) < sqrt(c_t * log(n))
}

rule_S <- function(X, rn){
    n <- length(X)
    dns <- 2^(0:rn)
    Qks <- sapply(dns, function(y) Q(y, X)$Q.value - y*log(n))
    S <- which.max(Qks)
    2^(S-1)
}

rule_A <- function(X, rn){
    n <- length(X)
    dns <- 2^(0:rn)
    Qks <- sapply(dns, function(y) Q(y, X)$Q.value - y*2)
    A <- which.max(Qks)
    2^(A-1)
}

rule_M <- function(X, rn){2^rn}

rule_T.M <- function(X, rn, c_t=2.4){
    n <- length(X)
    dns <- 2^(0:rn)
    Qks <- sapply(dns, function(y) Q(y, X)$Q.value - I_func(y, X, c_t)*log(n)*y)
    T.M <- which.max(Qks)
    2^(T.M-1) 
}

rule_T.A <- function(X, rn, c_t=2.4){
    n <- length(X)
    dns <- 2^(0:rn)
    Qks <- sapply(dns, function(y) Q(y, X)$Q.value - (I_func(y, X, c_t)*log(n)*y + (1-I_func(y, X, c_t))*2*y))
    T.A <- which.max(Qks)
    2^(T.A-1) 
}
