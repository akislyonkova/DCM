library("rstan") 
options(mc.cores = 4)


wd = getwd()
print(wd)
if (wd != '/home/akislyonkova/DCM/fdcm'){
    setwd('/home/akislyonkova/DCM/fdcm')
}
getwd()
respMatrix <- read.table("FTI.txt") 
#head(respMatrix)

inilist1<-list(Vc=c(rep(0.25,4))) 
inilist2<-list(Vc=c(rep(0.25,4)))

model <- stan_model("./FDCM_FTI.stan") 
date <- Sys.Date()

start.time <- Sys.time()
fdcm <- sampling(model,  
                  data=list(Y=respMatrix, 
                  Ns=4, 
                  Np=nrow(respMatrix), 
                  Ni=ncol(respMatrix), 
                  init=list(inilist1, inilist2), 
                  Nc=16), 
                  iter=6000, chains=2)  
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

save(fdcm, 'fdcm_FTI.rda') 
FDCM <- as.data.frame(summary(fdcm)$summary) 
#write.table(FDCM,file = paste('FDCM_FTI', '.txt', sep='')) 

converged <- sum(na.omit(FDCM$Rhat) > 1.05)
if (converged == 0) {
  print("converged")
}