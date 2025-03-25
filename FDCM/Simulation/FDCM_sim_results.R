
cell <- 2
n_sim <- 20 
rows_start <- 9 
rows_end <- 89 

file_name <- paste("cell", cell, "_param.txt", sep="")
fdcm_param <- read.table(file_name)
rm(fdcm_param)


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

hist(bias$raw_bias)

bias$rmse <- NA 
bias$rmse <- sqrt((rowSums(sim_param_dif))^2/(n_sim-1)) 
hist(bias$rmse) 
