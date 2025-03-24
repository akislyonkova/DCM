library("rstan") 
options(mc.cores = 4)


wd = getwd()
print(wd)
if (wd != '/home/akislyonkova/DCM/fdcm'){
    setwd('/home/akislyonkova/DCM/fdcm')
}
getwd()
respMatrix <- read.table("FTI.txt")  

inilist1 <- list(Vc = c(rep(0.25, 4)), step2_ID1 = 0.5, step2_ID3 = 0.5, step2_ID4 = 0.5)  # nolint: line_length_linter.
inilist2 <- list(Vc = c(rep(0.25, 4)), step2_ID1 = 0.5, step2_ID3 = 0.5, step2_ID4 = 0.5)  # nolint: line_length_linter.

model <- stan_model("./RSDM_FTI.stan") 
date <- Sys.Date()

start.time <- Sys.time()
rsdm <- sampling(model,  
                  data = list(Y = respMatrix, 
                  Ns = 4, 
                  Np = nrow(respMatrix), 
                  Ni = ncol(respMatrix), 
                  init = list(inilist1, inilist2), 
                  Nc = 16), 
                  iter = 6000, chains = 2)  
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

save(rsdm, file =  paste('rsdm_FTI', '.rda', sep=''))

converged <- sum(na.omit(RSDM$Rhat) > 1.05)
if (converged == 0) {
  print("converged")
}