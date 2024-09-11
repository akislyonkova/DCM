library(rstan) # library for stan
options(mc.cores = parallel::detectCores() / 2)
library(readxl)

Q <- as.matrix(read_excel("Q.xlsx"))

dark3 <- read.csv("sd3_data.csv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
dark3 <- dark3[sample(nrow(dark3), 1000), ]
respMatrix <- dark3[,-c(28:29)]
inilist1<-list(Vc=c(rep(0.33,3))) # creates the initial probabilities for 4 classes 
inilist2<-list(Vc=c(rep(0.33,3)))


# compiling a model
fdcm_model<-stan_model("FDCM.stan") # compiles a model into a formal class stanmodel

# suggested code to estimate a model
estimated_fdcm<-sampling(rrdm_model_simp, # specifies the model 
                         data=list(Y=respMatrix, Ns=5, 
                         Np=nrow(respMatrix), 
                         Ni=ncol(respMatrix), 
                         init=list(inilist1, inilist2), 
                         Nc=8), # specifies the data
                         iter=1, chains=2) # specifies the number of chains and iterations 

FDCM<-summary(estimated_fdcm)$summary # extracts model estimates and saves them into a data set 
save(estimated_fdcm, file = "FDCMest.rda") # saves the results into a stanfit object
#export(FDCM,"RRDMest_ordmdat.xlsx") # exports the dataset 