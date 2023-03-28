library("rstan")
options(mc.cores = parallel::detectCores() / 2)

library("rio")
respMatrix<-import("ordmdat.csv")
Q<-as.matrix(import("Q.xlsx"))
n_attr<-dim(Q)[2]
PS<-t(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) # profile set 
PfbyI<-Q %*% PS # profile by item matrix, weight matrix 

inilist1<-list(Vc=c(rep(0.25,4)))
inilist2<-list(Vc=c(rep(0.25,4)))

# compiling a model
nrdm_model_simp <- stan_model("NRDM_simplified.stan") # compiles a model into a formal class stanmodel

# suggested code to estimate a model
estimated_nrdm_ordmdat<-sampling(nrdm_model_simp, # specifies the model
                                 data = list(Y=respMatrix, Ns=5, Np=nrow(respMatrix), Ni=ncol(respMatrix), init=  list(inilist1,inilist2), Nc=16, W=PfbyI, Nstep=4), # specifies the data
                                 iter = 100,chains=1) # specifies the number of chains and iterations


NRDM_ordmdat<-summary(estimated_nrdm_ordmdat)$summary # extracts model estimates and saves them into a data set 
save(estimated_nrdm_ordmdat, file = "RSDMest_ordmdat.rda") # saves the results into a stanfit object
export(NRDM_ordmdat,"RRDMest_ordmdat.xlsx") # exports the dataset 


#testing

