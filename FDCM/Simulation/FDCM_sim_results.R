### Cell 1

n_sim <- 3  
rows_start <- 9 
rows_end <- 89 

fdcm_param <- read.table("cell1_param.txt") 


sim_param <- NA 
for (i in 1:n_sim) { 
  sim_name <- sprintf("FDCM_sim%i2025-02-17.txt", i)
  sim_param <- cbind(sim_param,read.table(sim_name)[c(rows_start:rows_end),1])
}
sim_param <- sim_param[,-1]  


### Calculating raw bias
fdcm_param_n <- data.frame(rep(fdcm_param, n_sim)) # duplicating the columns with the original parameters
sim_param_dif <- (sim_param - fdcm_param_n) # calulating the differences 
for (i in 1:n_sim) {
  colnames(sim_param_dif)[i] <- paste("sim", i, sep='') # renaming the columns 
}

bias <- data.frame(rowSums(sim_param_dif)/n_sim) # calculating raw bias 
colnames(bias) <- "raw_bias" # renaming the column

