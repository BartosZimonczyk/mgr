source("src/basic_functions.R")
source("src/choice_rules.R")

Ss <- c()
As <- c()
Ms <- c()
T.Ms <- c()
T.As <- c()

for(i in 1:20){
    X <- rnorm(50, 0, 1)
    Ss[i] <- rule_S(X, 5)
    As[i] <- rule_A(X, 5)
    Ms[i] <- rule_M(X, 5)
    T.Ms[i] <- rule_T.M(X, 5)
    T.As[i] <- rule_T.A(X, 5)
}

As
T.As
Ss
T.Ms
Ms

