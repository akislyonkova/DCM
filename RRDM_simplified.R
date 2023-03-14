library("rstan")
#options(mc.cores = parallel::detectCores() / 2)


library("rio") #library for function import 
respMatrix = import("data.csv") # load the data set
Q = matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1), nrow = 8) # creates a Q matrix 
n_attr = dim(Q)[2] # calculates the number of attributes based on Q matrix
PS = t(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) # profile set 
PfbyI = Q %*% PS # profile by item matrix, weight matrix 

inilist1 = list(Vc=c(rep(0.25,4))) # creates the initial probabilities for 4 classes 
inilist2 = list(Vc=c(rep(0.25,4)))

# compiling a model
rrdm_model_simp = stan_model("RRDM_simplified.stan") # compiles a model into a formal class stanmodel

# suggested code to estimate a model
estimated_model_simp = sampling(rrdm_model_simp, # specifies the model 
                           data=list(Y=respMatrix, Ns=5, Np=nrow(respMatrix), Ni=ncol(respMatrix), init=list(inilist1, inilist2), Nc=4, W=PfbyI, Nr=4), # specifies the data
                           iter=100, chains=2) # specifies the number of chains and iterations 

save(estimated_model_simp, file = "RRDMest_simp.rda") # saves the results into a stanfit object 
RRDM_simp = summary(estimated_model_simp)$summary # extracts model estimates and saves them into a data set 
export(RRDM_simp,"RRDMest_simp.xlsx") # exports the dataset 



# testing the loop 
lam = c(0,0.25,0.25,0.5) # intercepts for steps 
Ns = 5 # number of response options  
Nr = Ns - 1 # number of steps 
Ni = 8 # number of items 
Nc = 4 # number of classes (latent profiles)
lambda0 = c(0,0.25,0.25,0,0.125,0.125,0.125,0.5) # intercepts for the items 
lambda1 = c(0,0.25,0.25,0,0.125,0.125,0.125,0.5) # main effects for the items 
Q = matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1), nrow = 8) # Q matrix 

n_attr = dim(Q)[2]
PS = t(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) # profile set 
PfbyI = Q %*% PS # profile by item matrix, weight matrix 

PImat = array(0, dim = c(Ni, Nc, Ns)) # creates a 3D matrix NixNcxNs


current_sum = 0 # set the current sum parameter to 0
for (step in 1:Nr) {
  new_sum = lam[step] 
  current_sum = current_sum + new_sum
  for (item in 1:Ni) {
    for (pf in 1:Nc){
      W = PfbyI[item,pf]
      PImat[item, pf, step+1] = (step*(lambda0[item] + lambda1[item]*W) - current_sum)
    }
    
  }
}

PImat

