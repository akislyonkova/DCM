n <- 7

date <- Sys.Date()

library("rstan") 
options(mc.cores = parallel::detectCores())

filename <- paste('sim', n, '.txt', sep='')
respMatrix <- read.table(filename)
print(filename)
dim(respMatrix)

inilist1 <- list(Vc=c(rep(0.3,3))) 
inilist2 <- list(Vc=c(rep(0.3,3)))

fdcm_model <- stan_model("FDCM.stan") 

start.time <- Sys.time()
fdcm <- sampling(fdcm_model,  
                  data=list(Y=respMatrix, 
                  Ns=5, 
                  Np=nrow(respMatrix), 
                  Ni=ncol(respMatrix), 
                  init=list(inilist1, inilist2), 
                  Nc=8), 
                  iter=6000, chains=2)  
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

save(fdcm, file =  paste('fdcm_sim', n, '.rda', sep='')) 
FDCM <- as.data.frame(summary(fdcm)$summary) 
write.table(FDCM,file = paste('FDCM_sim', n, '.txt', sep='')) 

converged <- sum(na.omit(FDCM$Rhat) > 1.05)




