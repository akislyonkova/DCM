library("rstan")
options(mc.cores = parallel::detectCores() / 2)

respMatrix <- read.csv("discdat.csv")

n_attr <- dim(Q)[2]
PS <- t(expand.grid(replicate(n_attr, 0:1, simplify = FALSE)))
PfbyI <- Q %*% PS 

inilist1<-list(Vc=c(rep(0.25,4)))
inilist2<-list(Vc=c(rep(0.25,4)))

rsdm_model <- stan_model("RSDM.stan") 

# suggested code to estimate a model
date <- Sys.Date()

start.time <- Sys.time()
rsdm <- sampling(rsdm_model,
                 data = list(Y=respMatrix, 
                             Ns=5, 
                             Np=nrow(respMatrix), 
                             Ni=ncol(respMatrix), 
                             init=list(inilist1,inilist2), 
                             Nc=16, 
                             W=PfbyI, 
                             Nstep=4), 
                 iter = 6000, chains=2) 
time.taken <- end.time - start.time

save(rsdm, file =  paste('rsdm_', date, '.rda', sep=''))  
RSDM <- as.data.frame(summary(rsdm)$summary) 
write.table(RSDM, file =  paste('RSDM', date,'.txt',sep='')) 

converged <- sum(na.omit(RSDM$Rhat) > 1.1)

