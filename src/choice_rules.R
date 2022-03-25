source("src/basic_functions.R")

I_func <- function(dn, X, c_t=2.4){
    n <- length(X)
    Ls <- sapply(1:dn, L, X)
    max(abs(Ls)) < sqrt(c_t * log(n))
}

rule_S <- function(X, rn){
    n <- length(X)
    dns <- 2^(0:rn)
    Qks <- sapply(dns, function(y) Q(y, X) - y*log(n))
    S <- which.max(Qks)
    2^(S-1)
}

rule_A <- function(X, rn){
    n <- length(X)
    dns <- 2^(0:rn)
    Qks <- sapply(dns, function(y) Q(y, X) - y*2)
    A <- which.max(Qks)
    2^(A-1)
}

rule_M <- function(X, rn){32}

rule_T.M <- function(X, rn){
    n <- length(X)
    dns <- 2^(0:rn)
    Qks <- sapply(dns, function(y) Q(y, X) - I_func(y, X)*log(n))
    T.M <- which.max(Qks)
    2^(T.M-1) 
}

rule_T.A <- function(X, rn){
    n <- length(X)
    dns <- 2^(0:rn)
    Qks <- sapply(dns, function(y) Q(y, X) - (I_func(y, X)*log(n) + (1-I_func(y, X))*2))
    T.A <- which.max(Qks)
    2^(T.A-1) 
}
