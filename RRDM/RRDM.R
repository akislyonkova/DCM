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
rrdm4<-sampling(rrdm_model,  
                  data=list(Y=respMatrix, 
                  Ns=5, 
                  Np=nrow(respMatrix), 
                  Ni=ncol(respMatrix), 
                  init=list(inilist1, inilist2), 
                  Nc=8), 
                  iter=6000, chains=2)  
end.time <- Sys.time()
time.taken <- end.time - start.time

save(rrdm4, file =  paste('rrdm', n,'_', date, sep=''))  
RRDM_4<-as.data.frame(summary(rrdm4)$summary) 
#write.table(RRDM_4,'RRDMsim4.txt') 
write.table(RRDM_4, file =  paste('RRDMsim', n,'.txt',sep='')) 

converged <- sum(na.omit(RRDM_4$Rhat) > 1.1)

rows_start <- 9 
rows_end <- 80  
rrdm_param <- read.table("cell1_param.txt") 
RRDM <- read.table("RRDMsim4.txt") 
sim_param <- data.frame(RRDM[c(rows_start:rows_end),1]) 
bias <- rrdm_param - sim_param
colnames(bias) <- 'raw'
hist(bias$raw)
