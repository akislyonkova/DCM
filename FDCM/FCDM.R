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


#data https://openpsychometrics.org/
dark3 <- read.csv("sd3_data.csv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
dark3 <- dark3[-4864,]                                 # there is one NA value in that row
dark3_clean <- dark3[!apply(dark3 == 0, 1, any), ]     # delete any rows that contain "0" (no such value in codebook)
dark3_clean <- dark3_clean[,-c(28:29)]                 # delete the last two rows containing other info
respMatrix <- dark3_clean[sample(nrow(dark3_clean), 1000), ] # from this clean data select 1000 participants
sum(is.na(respMatrix))
write.table(respMatrix, "dark3.txt")                   # save the result

