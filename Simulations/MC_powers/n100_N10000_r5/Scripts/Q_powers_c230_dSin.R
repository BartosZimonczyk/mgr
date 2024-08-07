source("src/Q_test.R")
library(tidyverse)
options(bitmapType='cairo')

get_this_file <- function(){
  commandArgs() %>% 
    tibble::enframe(name=NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
}

# automating simulations by changing control to naming
# given folders and files

path <- get_this_file()
print(path)
path_splitted <- strsplit(path, "/")[[1]]
this_folder_name <- path_splitted[7]
this_file_name <- path_splitted[9]

parameters <- strsplit(this_folder_name, "_")
print(parameters)
parameters <- sapply(parameters, substring, 2)
parameters <- sapply(parameters, as.double)
parameters <- unname(parameters)

n <- parameters[1]
N <- parameters[2]
r <- parameters[3]
distribution <- unlist(strsplit(this_file_name, "_"))[4]
distribution <- substring(distribution, 2, nchar(distribution) - 2)
c_t <- as.double(substr(this_file_name, 11, 13)) / 100

#testing by hand
# n <- 50
# N <- 100
# r <- 5
# c_t <- 2.3
# distribution <- "LC"
# this_file_name <- "Q_powers_c230_dLC.R"


cat("Sanity check of hyperparameters: \n")
cat(paste("n:\t", n, "\n"))
cat(paste("N:\t", N, "\n"))
cat(paste("r:\t", r, "\n"))
cat(paste("c:\t", c_t, "\n"))
cat(paste("d:\t", distribution, "\n"))
cat("\n")

# load empirical quantiles for each rule
critical_values <- read.csv(
  paste("Simulations/MC_quantiles/n", n, "_N", 10000, "_r", r, "/Tables/Q_quantiles_c", (c_t*100), ".csv", sep="")
)[["X0.95"]]

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

dists_list <- list()

EV <- function(x, theta){exp(x-theta - exp(x-theta))}
new_beta <- function(x, theta){0.3*(dbeta(x - 1, 1, 2) + dbeta(x, 2, 1)) + 0.4*dbeta(x-0.5, 1, theta)}
mix_beta <- function(x, theta){0.1*(dbeta(x-1,1,2) + dbeta(x,2,1)) + 0.8*dbeta(x+2^(-1/theta) - 1, 1,theta)}
new_sin <- function(x, theta, j){0.5 + theta*sin(pi * j *x)}
Lehm <- function(x, theta){(theta * 0.5^theta * (x+1)^(theta-1))*(abs(x) <= 1)}
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
en <- function(x){(dnorm(x+1)*(x < -1) + dnorm(0)*( x >= -1 && x <= 1) + dnorm(x-1)*(x > 1))/(1+2*dnorm(0))}
ENB <- function(x, theta){
  m <- (theta - 1/3)/(theta+2 - 2/3)
  0.2*en(x) + 0.8*m*dbeta(m*x+m, theta, 2)
}

dists_list[["EV"]] <- EV
dists_list[["newbeta"]] <- new_beta
dists_list[["mixbeta"]] <- mix_beta
dists_list[["newsin"]] <- new_sin
dists_list[["Lehm"]] <- Lehm
dists_list[["N2B2"]] <- N2B2
dists_list[["LC"]] <- LC
dists_list[["NC2"]] <- NC2
dists_list[["Chi2"]] <- Chi2
dists_list[["NC"]] <- NC
dists_list[["B3"]] <- B3
dists_list[["rTuk"]] <- rTuk
dists_list[["ENB"]] <- ENB
#########
# liczenie mocy

A_crit <-   critical_values[1]
T.A_crit <- critical_values[2]
S_crit <-   critical_values[3]
T.M_crit <- critical_values[4]
M_crit <-   critical_values[5]

left <- -10
right <- 10
x_axis <- seq(left, right, 0.01)

A <- c()
T.A <- c()
S <- c()
T.M <- c()
M <- c()

building_A <- matrix(nrow=N, ncol=2^r)
building_T.A <- matrix(nrow=N, ncol=2^r)
building_S <- matrix(nrow=N, ncol=2^r)
building_T.M <- matrix(nrow=N, ncol=2^r)
building_M <- matrix(nrow=N, ncol=2^r)

chosen_k_A <- c()
chosen_k_T.A <- c()
chosen_k_S <- c()
chosen_k_T.M <- c()
chosen_k_M <- c()

set.seed(73)
for(i in 1:N){
  # cat("Simulation no.", i, "\n")
  
  if(distribution == "B3"){
    theta <- 2.5
    X <- ranydist(
      n, left, right, 
      dists_list[[distribution]], 
      max(dists_list[[distribution]](x_axis, theta)), 
      theta
    )
    X <- X - median(X)
  }else if(distribution == "ENB"){
    theta <- 6
    X <- ranydist(
      n, left, right, 
      dists_list[[distribution]], 
      max(dists_list[[distribution]](x_axis, theta)), 
      theta
    )
    X <- X - median(X)
  }else if(distribution == "N2B2"){
    theta <- 12
    X <- ranydist(
      n, left, right, 
      dists_list[[distribution]], 
      max(dists_list[[distribution]](x_axis, theta)), 
      theta
    )
    X <- X - median(X)
  }else if(distribution == "NC2"){
    theta <- 1.4
    X <- ranydist(
      n, left, right, 
      dists_list[[distribution]], 
      max(dists_list[[distribution]](x_axis, theta)), 
      theta
    )
    X <- X - median(X)
  }else if(distribution == "Chi2"){
    theta <- 4
    X <- ranydist(
      n, left, right, 
      dists_list[[distribution]], 
      max(dists_list[[distribution]](x_axis, theta)), 
      theta
    )
    X <- X - median(X)
  }else if(distribution == "LC"){
    theta <- 10
    X <- ranydist(
      n, left, right, 
      dists_list[[distribution]], 
      max(dists_list[[distribution]](x_axis, theta)), 
      theta
    )
    X <- X - median(X)
  }else if(distribution == "NC"){
    theta <- 3.4
    X <- ranydist(
      n, left, right, 
      dists_list[[distribution]], 
      max(dists_list[[distribution]](x_axis, theta)), 
      theta
    )
    X <- X - median(X)
  }else if(distribution == "Sin"){
    left <- -1
    right <- 1
    x_axis <- seq(left, right, 0.01)
    X <- ranydist(n, left, right, new_sin, max(new_sin(x_axis, 0.5, 8)), 0.5, 8)
    X <- X - median(X)
  }else if(distribution == "Cauchy"){
    X <- rcauchy(n, 0.4)
  }else if(distribution == "Logis"){
    X <- rlogis(n, 0.4)
  }else if(distribution == "Norm"){
    X <- rnorm(n, 0.4)
  }else if(distribution == "NormShift1"){
    X <- rnorm(n) - rnorm(n, 0.5, 2)
  }else if(distribution == "NormShift2"){
    X <- rnorm(n) - rcauchy(n, 1, 1)
  }else if(distribution == "NormShift3"){
    X <- rnorm(n) - runif(n, -1, 2)
  }else if(distribution == "Chi"){
    X <- rchisq(n, 9)
  }else if(distribution == "EV"){
    theta <- 0.367
    X <- ranydist(
      n, left, right, 
      dists_list[[distribution]], 
      max(dists_list[[distribution]](x_axis, theta)), 
      theta
    )
    X <- X - median(X)
  }else if(distribution == "Lehm"){
    theta <- 1.2
    left <- -1
    right <- 1
    x_axis <- seq(left, right, 0.01)
    X <- ranydist(
      n, left, right, 
      dists_list[[distribution]], 
      max(dists_list[[distribution]](x_axis, theta)), 
      theta
    )
    X <- X - median(X)
  }else if(distribution == "Tuk01"){
    X <- rTuk(n, 0.1, 0.4)
    X <- X - median(X)
  }else if(distribution == "Tuk7"){
    X <- rTuk(n, 7, 1.6)
    X <- X - median(X)
  }else if(distribution == "HO"){
    X <- runif(n, -1, 1)
  }
  
  this_A <-   Q_test(X, r, rule = "A")
  this_T.A <- Q_test(X, r, rule = "T.A", c_t)
  this_S <-   Q_test(X, r, rule = "S")
  this_T.M <- Q_test(X, r, rule = "T.M", c_t)
  this_M <-   Q_test(X, r, rule = "M")
  
  A[i] <-   this_A$Q.test
  T.A[i] <- this_T.A$Q.test
  S[i] <-   this_S$Q.test
  T.M[i] <- this_T.M$Q.test
  M[i] <-   this_M$Q.test
  
  building_A[i, ] <- this_A$Ls
  building_T.A[i, ] <- this_T.A$Ls
  building_S[i, ] <- this_S$Ls
  building_T.M[i, ] <- this_T.M$Ls
  building_M[i, ] <- this_M$Ls
  
  chosen_k_A[i] <- this_A$k
  chosen_k_T.A[i] <- this_T.A$k
  chosen_k_S[i] <- this_S$k
  chosen_k_T.M[i] <- this_T.M$k
  chosen_k_M[i] <- this_M$k
}

df_H0 <- data.frame('A' = round(mean(A <= A_crit), 3)*100,
                    'T.A' = round(mean(T.A <= T.A_crit), 3)*100,
                    'S' = round(mean(S <= S_crit), 3)*100,
                    'T.M' = round(mean(T.M <= T.M_crit), 3)*100,
                    'M' = round(mean(M <= M_crit), 3)*100)

df <- data.frame('A' = round(mean(A > A_crit), 3)*100,
                 'T.A' = round(mean(T.A > T.A_crit), 3)*100,
                 'S' = round(mean(S > S_crit), 3)*100,
                 'T.M' = round(mean(T.M > T.M_crit), 3)*100,
                 'M' = round(mean(M > M_crit), 3)*100)

mean_no_zeros <- function(V){
  if(sum(V) == 0){
    0
  }else{
    mean(V[V != 0])
  }
}

df_ls <- data.frame(
  "A" = apply(building_A, 2, mean, na.rm=TRUE),
  "T.A" = apply(building_T.A, 2, mean, na.rm=TRUE),
  "S" = apply(building_S, 2, mean, na.rm=TRUE),
  "T.M" = apply(building_T.M, 2, mean, na.rm=TRUE),
  "M" = apply(building_M, 2, mean, na.rm=TRUE)
)

df_ls_no_zeros <- data.frame(
  "A" = apply(building_A, 2, mean_no_zeros),
  "T.A" = apply(building_T.A, 2, mean_no_zeros),
  "S" = apply(building_S, 2, mean_no_zeros),
  "T.M" = apply(building_T.M, 2, mean_no_zeros),
  "M" = apply(building_M, 2, mean_no_zeros)
)

write.csv(
  format(df, nsmall=3, digits=3),
  paste("Simulations/MC_powers/", this_folder_name, "/Tables/Q_powers_c", round(c_t*100, 0), "_d", distribution, ".csv", sep="")
)

write.csv(
  format(df_ls, nsmall=3, digits=3),
  paste("Simulations/MC_powers/", this_folder_name, "/Tables/Q_mean_ls_c", round(c_t*100, 0), "_d", distribution, ".csv", sep="")
)

rule_names <- c("A", "S.A", "S", "S.M", "M")
order_of_ls <- c(17,9,18,5,19,10,20,3,21,11,22,6,23,12,24,2,25,13,26,7,27,14,28,4,29,15,30,8,31,16,32,1)

#png(paste("Simulations/MC_powers/", this_folder_name, "/Plots/Mean_ls_c", round(c_t*100, 0) ,"_d", distribution, ".png", sep=""), height=1000, width=800)
# par(mfrow=c(5,1))
# for(i in 1:5){
#   b = barplot(
#     names.arg = round(p(order_of_ls), 2),
#     df_ls_no_zeros[order_of_ls, i],
#     ylim = c(0, max(df_ls_no_zeros)+1.1),
#     yaxt='n',
#     main=paste("Barplot of mean values of lj for rule", rule_names[i]),
#     xlab = expression(paste("The end point of interval in wich we are checking assymetry i.e., [0, ", phi, "(j)], in increasing order", sep="")),
#     ylab = "Mean",
#     las=2
#   )
#   text(b, df_ls_no_zeros[order_of_ls, i]+0.85, labels=paste(as.character(round(df_ls_no_zeros[order_of_ls ,i], 2))))
# }

min_or_zero <- function(x) {
  if(min(x) > 0){
    return(0)
  }else{
    return(min(x))
  }
}
zero_neg_values_and_adapt <- function(x) {
  ind <- which(x < 0.01)
  if(identical(ind, integer(0))){
    x + 0.0
  }else{
    x_copy <- x
    x[ind] <- 0.1
    x[-ind] <- x[-ind] + 0.1
    # x + abs(min_or_zero(x_copy))
    x
  }
}
png(paste("Simulations/MC_powers/", this_folder_name, "/Plots/Mean_ls_c", round(c_t*100, 0) ,"_d", distribution, ".png", sep=""), height=900, width=800)
par(mfrow=c(3,1))
for(i in 3:5){
  b = barplot(
    names.arg = round(p(order_of_ls), 2),
    df_ls[order_of_ls, i] / sqrt(n),
    ylim = c(min(df_ls/sqrt(n)), max(df_ls/sqrt(n))+0.5),
    yaxt = 'n',
    main = bquote(paste("Barplot of mean values of ", gamma[j], " for rule" ~ .(rule_names[i]))),
    xlab = bquote(paste("The end point of interval in wich we are checking assymetry i.e., [0, ", phi, "(j)], in increasing order", sep="")),
    ylab = "Mean",
    las=2,
    #offset=abs(min_or_zero(df_ls[order_of_ls, i]/sqrt(n)))
    offset=0
  )
  text(b, zero_neg_values_and_adapt(df_ls[order_of_ls, i]/sqrt(n)), labels=paste(as.character(round(df_ls[order_of_ls ,i]/sqrt(n), 2))), srt=60)
}

dev.off()

png(paste("Simulations/MC_powers/", this_folder_name, "/Plots/Mean_Monly_ls_c", round(c_t*100, 0) ,"_d", distribution, ".png", sep=""), height=400, width=800)
par(mfrow=c(1,1))
i <- 5
b = barplot(
  names.arg = round(p(order_of_ls), 2),
  df_ls[order_of_ls, i] / sqrt(n),
  ylim = c(0, max(df_ls/sqrt(n))+1.1),
  yaxt = 'n',
  main = bquote(paste("Barplot of mean values of ", gamma[j], " for rule" ~ .(rule_names[i]))),
  xlab = bquote(paste("The end point of interval in wich we are checking assymetry i.e., [0, ", phi, "(j)], in increasing order", sep="")),
  ylab = "Mean",
  las=2,
  offset=abs(min_or_zero(df_ls[order_of_ls, i]/sqrt(n)))
)
text(b, zero_neg_values_and_adapt(df_ls[order_of_ls, i]/sqrt(n)), labels=paste(as.character(round(df_ls[order_of_ls ,i]/sqrt(n), 2))), srt=60)

dev.off()











