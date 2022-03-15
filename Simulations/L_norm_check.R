source("src/basic_functions.R")

set.seed(42)

MC_runs <- 10000
k <- 32

Ls <- matrix(nrow=MC_runs, ncol=k)
for(i in 1:MC_runs){
    cat('Randomizing...\t', i, '\n')
    X <- rnorm(50)
    Ls[i, ] <- sapply(1:k, L, X)
}

Lqs <- matrix(nrow=MC_runs, ncol=8)
Lq2s <- matrix(nrow=MC_runs, ncol=4)
for(i in 1:MC_runs){
    cat('Crunching quantiles...\t', i, '\n')
    Lqs[i,] <- quantile(Ls[i,], c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99))
    Lq2s[i,] <- quantile(Ls[i,]^2, c(0.9, 0.95, 0.975, 0.99))
}

write.csv(data.frame('L quantiles' = apply(Lqs, 2, mean),
                     'Norm quantiles' = qnorm(c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)),
            row.names = c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)),
        "Simulations/L_quantiles.csv")

write.csv(data.frame('L2 quantiles' = apply(Lq2s, 2, mean),
                     'Chisq quantiles' = qchisq(c(0.9, 0.95, 0.975, 0.99), df=1),
                     row.names = c(0.9, 0.95, 0.975, 0.99)),
          "Simulations/L2_quantiles.csv")

pvals <- c()
for(i in 1:MC_runs){
    pvals[i] <- shapiro.test(Ls[i,])$p.value
}
png('Images/pvals_shapiro_on_L.png')
hist(pvals)
dev.off()

