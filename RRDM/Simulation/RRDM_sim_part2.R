library("rstan")
options(mc.cores = 4)

wd = getwd()
print(wd)
if (wd != '/home/akislyonkova/DCM/rrdm_sim'){
    setwd('/home/akislyonkova/DCM/rrdm_sim/')
}
getwd()

for (n in 9:17) {
  filename <- paste('sim', n, '.txt', sep = '')
  print(filename)
  respMatrix <- read.table(filename)
  
  inilist1 <- list(Vc = c(rep(0.5,2))) 
  inilist2 <- list(Vc = c(rep(0.5,2)))
  
  model <- stan_model("RRDM10.stan") 
  
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
  
  # save(rrdm, file =  paste('rrdm_sim', n, '.rda', sep = '')) 
  RRDM <- as.data.frame(summary(rrdm)$summary) 
  write.table(RRDM,file = paste('RRDM_sim', n, '.txt', sep = '')) 

  converged <- sum(na.omit(RRDM$Rhat) > 1.05)
  if (converged == 0) {
    print("converged")
  }
  
  rm(list = ls())
} 
