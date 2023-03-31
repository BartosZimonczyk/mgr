source("src/basic_functions.R")
source("src/choice_rules.R")

Q_test <- function(X, r, rule, c_t=2.4){
  rule <- toupper(rule)
  if(rule == "A"){
    k <- rule_A(X, r)
  }else if(rule == "T.A"){
    k <- rule_T.A(X, r, c_t)
  }else if(rule == "S"){
    k <- rule_S(X, r)
  }else if(rule == "T.M"){
    k <- rule_T.M(X, r, c_t)
  }else if(rule == "M"){
    k <- rule_M(X, r)
  }else{
    stop(sprintf("The %s rule is not an implemented rule. Check avilable ones.", rule))
  }
  
  Q_all <- Q(k, X)
  output <- list(Q_all$Q.value, k, c(Q_all$Ls, rep(0, times=2^r - k)))
  names(output) <- c("Q.test", "k", "Ls")
  output
}