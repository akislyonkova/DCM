library("rstan") 
options(mc.cores = parallel::detectCores())

for (n in 1:25) {
  filename <- paste('sim', n, '.txt', sep = '')
  respMatrix <- read.table(filename)
  
  inilist1 <- list(Vc = c(rep(0.5,2))) 
  inilist2 <- list(Vc = c(rep(0.5,2)))
  
  model <- stan_model("RRDM40.stan") 
  
  start.time <- Sys.time()
  rrdm <- sampling(model,  
                   data = list(Y = respMatrix, 
                             Ns = 5, 
                             Np = nrow(respMatrix), 
                             Ni = ncol(respMatrix), 
                             init = list(inilist1, inilist2), 
                             Nc = 4), 
                             iter = 6000, chains = 2)  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  save(rrdm, file =  paste('rrdm_sim', n, '.rda', sep = '')) 
  RRDM <- as.data.frame(summary(rrdm)$summary) 
  write.table(RRDM,file = paste('RRDM_sim', n, '.txt', sep = '')) 
  
  rm(list = ls())
} 
