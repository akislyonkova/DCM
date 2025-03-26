library("rstan") 
options(mc.cores = parallel::detectCores())

respMatrix <- read.table("FTI.txt")  
respMatrix <- respMatrix[,c(1:3)]

inilist1 <- list(Vc = c(rep(0.5, 2))) 
inilist2 <- list(Vc = c(rep(0.5, 2)))

model <- stan_model("dcm_original.stan") 

start.time <- Sys.time()
fdcm <- sampling(model,  
                 data = list(Y=respMatrix, 
                             Ns = 4, 
                             Np = nrow(respMatrix), 
                             Ni = ncol(respMatrix), 
                             init = list(inilist1, inilist2), 
                             Nc = 4), 
                 iter = 100, chains=1)  
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

################################################################################################
model_try <- stan_model("dcm_speedup2.stan") 

start.time <- Sys.time()
fdcm <- sampling(model_try,  
                 data = list(Y=respMatrix, 
                             Ns = 4, 
                             Np = nrow(respMatrix), 
                             Ni = ncol(respMatrix), 
                             init = list(inilist1, inilist2), 
                             Nc = 4), 
                 iter = 100, chains=1)  
end.time <- Sys.time()
time.taken.speedup <- end.time - start.time
print(time.taken.speedup)


