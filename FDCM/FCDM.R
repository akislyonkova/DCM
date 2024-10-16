library(rstan) 
options(mc.cores = parallel::detectCores() / 2)

inilist1<-list(Vc=c(0.33,0.33,0.34)) 
inilist2<-list(Vc=c(0.33,0.33,0.34))
respMatrix <- read.table("dark3.txt") 

fdcm_model<-stan_model("FDCM.stan") 

# suggested code to estimate a model
estimated_fdcm<-sampling(fdcm_model, 
                         data=list(Y=respMatrix, 
                         Ns=5, 
                         Np=nrow(respMatrix), 
                         Ni=ncol(respMatrix), 
                         init=list(inilist1, inilist2), 
                         Nc=8), 
                         iter=1, chains=1)  

FDCM<-summary(estimated_fdcm)$summary  
save(estimated_fdcm, file = "FDCMest.rda") 



# dark3 <- read.csv("sd3_data.csv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# dark3 <- dark3[sample(nrow(dark3), 1000), ]
# respMatrix <- dark3[,-c(28:29)]
# respMatrix[respMatrix=="0"]<-1
# write.table(respMatrix, "dark3.txt")
