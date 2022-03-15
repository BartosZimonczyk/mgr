source("src/basic_functions.R")

set.seed(42)

MC_runs <- 1000
k <- 32

Ls <- matrix(nrow=MC_runs, ncol=k)
for(i in 1:MC_runs){
    cat('Randomizing...\t', i, '\n')
    X <- rnorm(50)
    Ls[i, ] <- sapply(1:k, L, X)
}

Lqs <- matrix(nrow=8, ncol=k)
Lq2s <- matrix(nrow=4, ncol=k)
for(j in 1:k){
    cat('Crunching quantiles...\t', i, '\n')
    Lqs[,j] <- quantile(Ls[,j], c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99))
    Lq2s[,j] <- quantile(Ls[,j]^2, c(0.9, 0.95, 0.975, 0.99))
}

df_lqs <- data.frame(Lqs,
                row.names = c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99))
colnames(df_lqs) <- paste('L_', 1:32, sep='')
df_lqs$Norm <- qnorm(c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99))

df_lq2s <- data.frame(Lq2s,
                     row.names = c(0.9, 0.95, 0.975, 0.99))
colnames(df_lq2s) <- paste('L2_', 1:32, sep='')
df_lq2s$Chisq <- qchisq(c(0.9, 0.95, 0.975, 0.99), df=1)

write.csv(df_lqs, 'Simulattions/L_all_quantiles.csv')
write.csv(df_lq2s, 'Simulations/L2_all_quantiles.csv')

png("Images/L_all_quantiles.png")
for(i in 1:8){
    if(i == 1){
        plot(1:32, df_lqs[i,1:32], col=i, ylim=c(-3, 3), main="L quantiles")
    }else{
        points(1:32, df_lqs[i,1:32], col=i)
    }
    lines(1:32, rep(df_lqs[i,33], 32), col=i)
}
dev.off()

png("Images/L2_all_quantiles.png")
for(i in 1:4){
    if(i == 1){
        plot(1:32, df_lq2s[i,1:32], col=i, ylim=c(2, 7), main="L2 quantiles")
    }else{
        points(1:32, df_lq2s[i,1:32], col=i)
    }
    lines(1:32, rep(df_lq2s[i,33], 32), col=i)
}
dev.off()


# proper normality test
pvals <- c()
for(j in 1:32){
    pvals[j] <- shapiro.test(Ls[,i])$p.value
}
pvals







