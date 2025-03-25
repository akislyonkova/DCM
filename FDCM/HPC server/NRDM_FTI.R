library("rstan") 
options(mc.cores = 4)


wd = getwd()
print(wd)
if (wd != '/home/akislyonkova/DCM/fdcm'){
    setwd('/home/akislyonkova/DCM/fdcm')
}
getwd()
respMatrix <- read.table("FTI.txt")  

inilist1 <- list(Vc = c(rep(0.25, 4)), 
    l1_23step3 = 1.5, l1_24step3 = 1.5, l1_36step3 = 1.5, 
    l1_41step3 = 1.5, l1_46step3 = 1.5, l1_47step3 = 1.5) 
inilist2 <- list(Vc = c(rep(0.25, 4)), 
    l1_23step3 = 1.5, l1_24step3 = 1.5, l1_36step3 = 1.5, 
    l1_41step3 = 1.5, l1_46step3 = 1.5, l1_47step3 = 1.5) 


nrdm_model <- stan_model("./NRDM_FTI.stan") 
date <- Sys.Date()

start.time <- Sys.time()
nrdm <- sampling(nrdm_model,  
                  data = list(Y=respMatrix, 
                  Ns = 4, 
                  Np=nrow(respMatrix), 
                  Ni = ncol(respMatrix), 
                  init = list(inilist1, inilist2), 
                  Nc = 16), 
                  iter = 6000, chains = 2)   
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

save(nrdm, file =  paste('nrdm_FTI', '.rda', sep='')) 
NRDM <- as.data.frame(summary(nrdm)$summary) 

converged <- sum(na.omit(NRDM$Rhat) > 1.05)
print(converged)
