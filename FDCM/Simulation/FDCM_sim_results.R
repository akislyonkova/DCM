
cell <- 4
n_sim <- 20 
n_i <- 27
rows_start <- 9 
rows_end <- 89 

file_name <- paste("cell", cell, "_param.txt", sep="")
fdcm_param <- read.table(file_name)


sim_param <- NULL
for (i in 1:n_sim) { 
  sim_name <- sprintf("FDCM_sim%i.txt", i)
  sim_param <- cbind(sim_param,read.table(sim_name)[c(rows_start:rows_end),1])
}
 
fdcm_param_n <- data.frame(rep(fdcm_param, n_sim)) 
sim_param_dif <- (sim_param - fdcm_param_n)  
for (i in 1:n_sim) {
  colnames(sim_param_dif)[i] <- paste("sim", i, sep='') 
}

bias <- data.frame(rowSums(sim_param_dif)/n_sim)  
colnames(bias) <- "raw_bias" 
rownames(bias) <- paste0("item", seq(1, n_i), "_", rep(c("i", "m", "d"), each = n_i))

bias$rmse <- NA 
bias$rmse <- sqrt((rowSums(sim_param_dif))^2/(n_sim-1)) 

hist(bias$raw_bias)
hist(bias$rmse) 
