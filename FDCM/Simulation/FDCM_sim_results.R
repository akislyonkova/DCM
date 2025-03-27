
cell <- 4        # cell number 
n_sim <- 20      # number of replications 
n_i <- 27        # number of items 
rows_start <- 9  # first parameter 
rows_end <- 89   # last parameter 
n_id <- 9        # number of items per dimension 

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

#Overview
hist(bias$raw_bias)
hist(bias$rmse) 

#Bias by parameter type (intercepts, main effects and dispersion for each attribute)
start_idx <- seq(from = 1, by = n_id, length.out = 9)
end_idx <- start_idx + n_id - 1

bias.by.param <- data.frame(
  mapply(function(start, end) bias[start:end, 1], start_idx, end_idx)
)
colnames(bias.by.param) <- paste0(rep(c("i", "m", "d"), each = 3), "_A", rep(1:3, 2))

file_name <- paste("bias summary, cell", cell, ".txt", sep="")
sink(file_name)
summary(bias.by.param[,])
sink()

rmse.by.param <- data.frame(
  mapply(function(start, end) bias[start:end, 2], start_idx, end_idx)
)
colnames(rmse.by.param) <- paste0(rep(c("i", "m", "d"), each = 3), "_A", rep(1:3, 2))

file_name <- paste("rmse summary, cell", cell, ".txt", sep="")
sink(file_name)
summary(rmse.by.param[,])
sink()



