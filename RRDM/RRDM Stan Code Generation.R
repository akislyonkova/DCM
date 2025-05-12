library(stringr)
library(rstan)

RRDM<-function(Qmatrix,scale.num,save.path=getwd(),save.name="RRDM"){
  nstep=scale.num-1
  n_attr  <- ncol(Qmatrix)
  n_items <- nrow(Qmatrix)
  PS <- t(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) 
  PfbyI <- Q %*% PS  
  nclass <- ncol(PfbyI) 
  
  
  #li_0 - item intercept, list with nitems length 
  li_0 <- rep(c("NA"), n_items)
  for (i in 1:n_items){
    li_0[i]<-paste('l',i,'I', sep='')
  }
  
  item_intercepts <- li_0
  
  li_1 <- rep(c("NA"), n_items)
  for (i in 1:n_items){
    li_1[i]<-paste('l',i,'M', sep='')
  }
  
  ls_0 <- matrix("NA", n_items,nstep)
  
  for (s in 1:nstep){
    ls_0[,s] <- paste("step",s, "_I", sep='')
    ls_0[1:20, s] <- paste(ls_0[1:20, s], "D1", sep='') # step parameters for dimension 1
    ls_0[21:40, s] <- paste(ls_0[21:40, s], "D2", sep='')
  }
  
  ls_0_cumul <- ls_0
  ls_0_cumul[,2] <- paste(ls_0[,1],'+',ls_0[,2],sep='')
  ls_0_cumul[,3] <- paste(ls_0[,1],'+',ls_0[,2],'+',ls_0[,3],sep='')
  ls_0_cumul[,4] <- paste(ls_0[,1],'+',ls_0[,2],'+',ls_0[,3],'+',ls_0[,4],sep='')
  
  ##################################################################################################################################  
  ls_0_unique <-unique(ls_0)
  
  Reparm<-array(rep(0,n_items*nclass*(scale.num)),dim = c(n_items,nclass,(scale.num))) # placeholder for the loop results 
  
  
  for (loops in 1:nstep){
    for(loopi in 1:n_items){
      for(loopc in 1:nclass){
        Reparm[loopi,loopc,1]<-paste('  PImat[',loopi,',',loopc,'][1]=0;\n',sep='')
        Reparm[loopi,loopc,loops+1]<-paste('  PImat[',loopi,',',loopc,'][',loops+1,']=', loops, '*(',li_0[loopi],
                                           '+', li_1[loopi], '*', PfbyI[loopi,loopc], 
                                           ")+" ,ls_0_cumul[loopi,loops],
                                           ';\n',sep='')
      }
      
    }
  }   
  
  
  # build a Frankenstein 
  
  Modelcontainer<-paste('vector[Nc] contributionsC;\n','    vector[Ni] contributionsI;\n\n',sep='')
  Parmprior<-paste(c(paste('//Prior\n'),
                     paste(li_0,'~normal(0,2)',';\n', sep=''),  
                     paste(li_1,'~normal(0,2)', ';\n', sep=''),
                     paste(ls_0_unique,'~normal(0,2)', ';\n', sep=''),
                     paste('Vc~dirichlet(rep_vector(2.0, Nc));', sep='')))
  
  #Likelihood Stan code
  Likelihood<-'
  \n
  //Likelihood
  for (iterp in 1:Np){
    for (iterc in 1:Nc){
      for (iteri in 1:Ni){
        contributionsI[iteri]= categorical_lpmf(Y[iterp,iteri]| softmax(((PImat[iteri,iterc]))));
      }
      contributionsC[iterc]=log(Vc[iterc])+sum(contributionsI);
    }
    target+=log_sum_exp(contributionsC);
  }
  '
  data.spec<-'
  data{
  int Np;
  int Ni;
  int Nc;
  int Ns;
  int Y[Np, Ni];
  }
  '
  #Parameter Specification
  parm.spec<-paste(c('parameters{
                     simplex[Nc] Vc;\n ',
                     paste('real',li_0,';\n '),
                     paste('real<lower=0>',li_1,';\n '),
                     paste('real<lower=0>',ls_0_unique,';\n '),
                     '}\n'),
                   collapse='')
  
  
  
  #Reparameter Specification
  
  transparm.spec<-paste(c('transformed parameters{
                          vector[Ns] PImat[Ni, Nc];\n',
                          paste0(unlist(Reparm)),'}\n')
                        ,collapse='')
  
  #Model Specification
  model.spec<-paste(c('\nmodel {\n',paste(c(Modelcontainer,Parmprior,Likelihood),sep=''),'\n}',sep=''))
  model.spec<-model.spec[!startsWith(str_remove_all(model.spec," "),"~")]
  
  generatedQuantities.spec<-'
  \n
  generated quantities {
  vector[Ni] contributionsI;
  matrix[Np,Nc] contributionsPC;
  //Posterior
  for (iterp in 1:Np){
    for (iterc in 1:Nc){
      for (iteri in 1:Ni){
        contributionsI[iteri]= categorical_lpmf(Y[iterp,iteri]| softmax(((PImat[iteri,iterc]))));
      }
      contributionsPC[iterp,iterc]=prod(exp(contributionsI));
    }
  }
}
  
  '
if (.Platform$OS.type == "unix") {
  filename = paste(paste(save.path,save.name,sep='/'),'.stan',sep='')
}else{
  filename = paste(paste(save.path,save.name,sep='\\'),'.stan',sep='')
}

sink(file= filename,append=FALSE)
cat(
  paste(c('   ',
          data.spec,parm.spec,transparm.spec,model.spec,generatedQuantities.spec)
  ))
sink(NULL)

}



Q <- matrix(c(rep(c(1,0),20),rep(c(0,1),20)), 40, 2, byrow = T) # 20 items and 2 attributes
RRDM(Q,5)



