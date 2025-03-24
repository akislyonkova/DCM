library("rstan") 
options(mc.cores = 4)


wd = getwd()
print(wd)
if (wd != '/home/akislyonkova/DCM/fdcm'){
    setwd('/home/akislyonkova/DCM/fdcm')
}
getwd()
respMatrix <- read.table("FTI.txt")  

inilist1 <- list(Vc = c(rep(0.25, 4)), l47M = 1, l25I = 0.1, l27I = 0.1) 
inilist2 <- list(Vc = c(rep(0.25, 4)))

model <- stan_model("./FDCM_FTI.stan") 
date <- Sys.Date()

start.time <- Sys.time()
fdcm <- sampling(model,  
                  data = list(Y=respMatrix, 
                  Ns = 4, 
                  Np=nrow(respMatrix), 
                  Ni = ncol(respMatrix), 
                  init = list(inilist1, inilist2), 
                  Nc = 16), 
                  iter = 6000, chains=2)  
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

save(fdcm, file =  paste('fdcm_FTI', '.rda', sep = ''))  
FDCM <- as.data.frame(summary(fdcm)$summary) 

converged <- sum(na.omit(FDCM$Rhat) > 1.05)
if (converged == 0) {
  print("converged")
}