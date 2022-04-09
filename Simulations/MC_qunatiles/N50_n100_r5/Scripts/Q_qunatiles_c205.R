source("src/Q_test.R")
set.seed(42)

N <- 100
n <- 50
c_t <- 2.05
r <- 5
quants <- c(0.948, 0.949, 0.95, 0.951, 0.952)

# by columns we have test with rules respectively
# A, T.A, S, T.M, M
Q_matrix <- matrix(nrow=N, ncol=5)

for(i in 1:N){
  cat(paste("Computing...\t Step", i, "\n"))
  X <- rnorm(n)
  Q_matrix[i,] <- c(
    Q_test(X, r, rule = "A")$Q.test,
    Q_test(X, r, rule = "T.A")$Q.test,
    Q_test(X, r, rule = "S")$Q.test,
    Q_test(X, r, rule = "T.M")$Q.test,
    Q_test(X, r, rule = "M")$Q.test
  )
}

# matrix with given quantiles for all rules
# rules in rows, quantiles in columns
final_matrix <- matrix(nrow=5, ncol=length(quants))
for(i in 1:5){
  final_matrix[i,] <- quantile(Q_matrix[,i], probs=quants, names=F)
}

output <- data.frame(final_matrix, row.names=c("A", "T.A", "S", "T.M", "M"))
colnames(output) <- quants

write.csv(output, "Simulations/MC_qunatiles/Tables/Q_quantiles_c205.csv")




