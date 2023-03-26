source("src/Q_test.R")
library(tidyverse)
options(bitmapType='cairo')
set.seed(42)

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
this_folder_name <- path_splitted[3]
this_file_name <- path_splitted[5]

parameters <- strsplit(this_folder_name, "_")
print(parameters)
parameters <- sapply(parameters, substring, 2)
parameters <- sapply(parameters, as.double)
parameters <- unname(parameters)

n <- parameters[1]
N <- parameters[2]
r <- parameters[3]
c_t <- as.double(substr(this_file_name, 14, 16)) / 100

# testing by hand
# n <- 50
# N <- 100
# r <- 5
# c_t <- 2.2

quants <- c(0.948, 0.949, 0.95, 0.951, 0.952)

cat("Sanity check of hyperparameters: \n")
cat(paste("n:\t", n, "\n"))
cat(paste("N:\t", N, "\n"))
cat(paste("r:\t", r, "\n"))
cat(paste("c:\t", c_t, "\n"))
cat("\n")

# by columns we have test with rules respectively
# A, T.A, S, T.M, M
Q_matrix <- matrix(nrow=N, ncol=5)
K_matrix <- matrix(nrow=N, ncol=5)

for(i in 1:N){
  # cat(paste("Computing...\t Step", i, "\n"))
  X <- rnorm(n, 0, 1)
  
  Q_result_A <- Q_test(X, r, rule = "A")
  Q_result_T.A <- Q_test(X, r, rule = "T.A", c_t)
  Q_result_S <- Q_test(X, r, rule = "S")
  Q_result_T.M <- Q_test(X, r, rule = "T.M", c_t)
  Q_result_M <- Q_test(X, r, rule = "M")
  
  Q_matrix[i,] <- c(
    Q_result_A$Q.test,
    Q_result_T.A$Q.test,
    Q_result_S$Q.test,
    Q_result_T.M$Q.test,
    Q_result_M$Q.test
  )
  
  K_matrix[i,] <- c(
    Q_result_A$k,
    Q_result_T.A$k,
    Q_result_S$k,
    Q_result_T.M$k,
    Q_result_M$k
  )
}

# matrix with given quantiles for all rules
# rules in rows, quantiles in columns
final_matrix <- matrix(nrow=5, ncol=length(quants))
for(i in 1:5){
  final_matrix[i,] <- quantile(Q_matrix[,i], probs=quants, names=F)
}

df_output <- data.frame(final_matrix, row.names=c("A", "T.A", "S", "T.M", "M"))
colnames(df_output) <- quants

write.csv(
  format(df_output, nsmall=3, digits=3),
  paste("Simulations/MC_quantiles/", this_folder_name, "/Tables/Q_quantiles_c", round(c_t*100, 0), ".csv", sep="")
)

rule_names <- c("A", "T.A", "S", "T.M", "M")

png(paste("Simulations/MC_quantiles/", this_folder_name, "/Plots/RulesHist_c", round(c_t*100, 0), ".png", sep=""), height=900, width=600)
par(mfrow=c(5,1))
for(i in 1:5){
  freqs = c(mean(K_matrix[,i] == 1),
            mean(K_matrix[,i] == 2),
            mean(K_matrix[,i] == 4),
            mean(K_matrix[,i] == 8),
            mean(K_matrix[,i] == 16),
            mean(K_matrix[,i] == 32))
  b = barplot( 
    freqs,
    names.arg = c("1", "2", "4", "8", "16", "32"),
    ylim = c(0,1.3),
    yaxt='n',
    main=paste("Histogram of values of rule", rule_names[i]),
    xlab = rule_names[i],
    ylab = "Density"
  )
  text(b, freqs+0.15, labels=paste(as.character(freqs*100), "%"))
}

dev.off()





