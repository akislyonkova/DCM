library("rstan") # library for stan
options(mc.cores = parallel::detectCores() / 2)
library("rio") #library for function 'import' 

respMatrix<-import("ordmdat.csv") # load the data set
Q<-as.matrix(import("Q.xlsx")) # load the Q matrix 
n_attr<-dim(Q)[2] # calculates the number of attributes based on Q matrix
PS<-t(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) # profile set 
PfbyI<-Q %*% PS # profile by item matrix, weight matrix 

inilist1<-list(Vc=c(rep(0.25,4))) # creates the initial probabilities for 4 classes 
inilist2<-list(Vc=c(rep(0.25,4)))

# compiling a model
rrdm_model_simp<-stan_model("RRDM_simplified.stan") # compiles a model into a formal class stanmodel

# suggested code to estimate a model
estimated_rrdm_ordmdat<-sampling(rrdm_model_simp, # specifies the model 
                           data=list(Y=respMatrix, Ns=5, Np=nrow(respMatrix), Ni=ncol(respMatrix), init=list(inilist1, inilist2), Nc=16, W=PfbyI, Nr=4), # specifies the data
                           iter=100, chains=2) # specifies the number of chains and iterations 

 
RRDM_ordmdat<-summary(estimated_rrdm_ordmdat)$summary # extracts model estimates and saves them into a data set 
save(estimated_rrdm_ordmdat, file = "RRDMest_simp.rda") # saves the results into a stanfit object
export(RRDM_ordmdat,"RRDMest_ordmdat.xlsx") # exports the dataset 




