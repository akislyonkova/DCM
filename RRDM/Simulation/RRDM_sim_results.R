library(rstan)
n_p <- 1000    # number of people
n_i <- 20      # number of items 
n_r <- 5       # number of response options
n_t <- n_r - 1 # number of thresholds
n_d <- 2       # number of dimensions
n_c <- 2^n_d   # number of profiles  
n_id <- 10     # number of items per dimension 
n_sim <- 1     # number of simulations 


rrdm_param <- read.table("RRDM_cell1_param.txt") 

rows_start <- 5
rows_end <- rows_start + 2*n_i + 8 - 1

sim_param <- NA 
for (i in 1:n_sim) { 
  sim_name <- sprintf("RRDM_sim%i.txt", i)
  sim_param <- cbind(sim_param,read.table(sim_name)[c(rows_start:rows_end),1])
}
sim_param <- sim_param[,-1] 

### Calculating raw bias
rrdm_param_n <- data.frame(rep(rrdm_param, n_sim)) 
sim_param_dif <- (sim_param - rrdm_param_n) 
for (i in 1:n_sim) {
  colnames(sim_param_dif)[i] <- paste("sim", i, sep='')  
}

bias <- data.frame(rowSums(sim_param_dif)/n_sim)  
colnames(bias) <- "raw_bias" 


# Model diagnostics
rrdm@model_pars
check_divergences(rrdm)


l0 <- paste0("l", rep(1:n_i), "I")
l1 <- paste0("l", rep(1:n_i), "M")
step <- c("step1_ID1", "step1_ID2", 
             "step2_ID1", "step2_ID2", 
             "step3_ID1", "step3_ID2", 
             "step4_ID1", "step4_ID2")
rrdm_param <- c("Vc", l0, l1, step)

for (p in rrdm_param){
  print(traceplot(rrdm, pars = p))
}
