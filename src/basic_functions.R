p <- function(j){
    if(any(j < 0)){
        0
    }else{
        (j == 1)/2 + (j > 1) * (2*j - 2 ^ ceiling(log2(j)) - 1) / (2 ^ (1+ceiling(log2(j))))   
    }
}

l <- function(u, j){
    pj <- p(j)
    1/sqrt(2*pj) * (1-pj < u & u < 1) - 1/sqrt(2*pj) * (0 < u & u < pj)
}

L <- function(j, X){
    n <- length(X)
    inputs <- (rank(c(X, -X)) - 0.5)[1:n] / 2 / n
    final <- sapply(inputs, l, j)
    sum(final)/sqrt(n)
}

Q <- function(X, k){
    sum(sapply(1:k, L, X)^2)
}