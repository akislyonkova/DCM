library("rstan") 
options(mc.cores = parallel::detectCores())

n<-4
load('rrdm_cell1.rda')
respMatrix<-cell1[[n]] 

inilist1<-list(Vc=c(rep(0.3,3))) 
inilist2<-list(Vc=c(rep(0.3,3)))


rrdm_model<-stan_model("RRDM.stan") 

date <- Sys.Date()

start.time <- Sys.time()
rrdm<-sampling(rrdm_model,  
                  data=list(Y=respMatrix, 
                  Ns=5, 
                  Np=nrow(respMatrix), 
                  Ni=ncol(respMatrix), 
                  init=list(inilist1, inilist2), 
                  Nc=8), 
                  iter=6000, chains=2)  
end.time <- Sys.time()
time.taken <- end.time - start.time

save(rrdm, file =  paste('rrdm', n,'_', date, sep=''))  
RRDM<-as.data.frame(summary(rrdm)$summary) 

write.table(RRDM, file =  paste('RRDMsim', n,'.txt',sep='')) 

converged <- sum(na.omit(RRDM$Rhat) > 1.1)

rows_start <- 9 
rows_end <- 80  
true_param <- read.table("cell1_param.txt") 
sim_param <- RRDM[c(rows_start:rows_end),1] 
bias <- true_param - sim_param
colnames(bias) <- 'raw'
hist(bias$raw)
