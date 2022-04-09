source("src/Q_test.R")

Ss_Q <- c()
As_Q <- c()
Ms_Q <- c()
T.Ms_Q <- c()
T.As_Q <- c()

Ss_rule <- c()
As_rule <- c()
Ms_rule <- c()
T.Ms_rule <- c()
T.As_rule <- c()

for(i in 1:20){
  cat("Computing... ", i, "\n")
  X <- rnorm(50)
  S <- Q_test(X, 5, "S")
  A <- Q_test(X, 5, "A")
  M <- Q_test(X, 5, "M")
  T.M <- Q_test(X, 5, "T.M")
  T.A <- Q_test(X, 5, "T.A")
  
  Ss_Q[i] <- S$Q.test
  As_Q[i] <- A$Q.test
  Ms_Q[i] <- M$Q.test
  T.Ms_Q[i] <- T.M$Q.test
  T.As_Q[i] <- T.A$Q.test
  
  Ss_rule[i] <- S$k
  As_rule[i] <- A$k
  Ms_rule[i] <- M$k
  T.Ms_rule[i] <- T.M$k
  T.As_rule[i] <- T.A$k
}

As_Q
T.As_Q
Ss_Q
T.Ms_Q
Ms_Q

As_rule
T.As_rule
Ss_rule
T.Ms_rule
Ms_rule


