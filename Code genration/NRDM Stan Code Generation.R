library(stringr)

NRDM <- function(Q,scale.num,save.path=getwd(),save.name="NRDM"){
  nstep=scale.num-1
  n_attr  <- ncol(Q)
  n_items <- nrow(Q)
  PS <- t(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) 
  PfbyI <- Q %*% PS 
  nclass <- ncol(PfbyI)  
  
  
  #li_0 - item intercept, matrix with n_row=nitems, n_col=nstep
  #li_1 - item main effects, matrix with n_row=nitems, n_col=nstep
  li_0 <- matrix(c("NA"), n_items, nstep)
  li_1 <- matrix(c("NA"), n_items, nstep)
  for (s in 1:nstep){
    for (i in 1:n_items){
      li_0[i,s]<-paste('l0_',i,'step',s, sep='') 
      li_1[i,s]<-paste('l1_',i,'step',s, sep='') 
    }
  }
  #creating a cumulative matrix with a sum of step intercepts for each item 
  li_0_sum <- li_0
  li_0_sum[,2] <- paste(li_0[,1],'+',li_0[,2],sep='')
  li_0_sum[,3] <- paste(li_0[,1],'+',li_0[,2],'+',li_0[,3],sep='')
  li_0_sum[,4] <- paste(li_0[,1],'+',li_0[,2],'+',li_0[,3],'+',li_0[,4],sep='')
  li_0_sum[,5] <- paste(li_0[,1],'+',li_0[,2],'+',li_0[,3],'+',li_0[,4],'+',li_0[,5],sep='')
  li_0_sum[,6] <- paste(li_0[,1],'+',li_0[,2],'+',li_0[,3],'+',li_0[,4],'+',li_0[,5],'+',li_0[,6],sep='')
  
  #creating a cumulative matrix with a sum of step main effects for each item 
  li_1_sum <- li_1
  li_1_sum[,2] <- paste(li_1[,1],'+',li_1[,2],sep='')
  li_1_sum[,3] <- paste(li_1[,1],'+',li_1[,2],'+',li_1[,3],sep='')
  li_1_sum[,4] <- paste(li_1[,1],'+',li_1[,2],'+',li_1[,3],'+',li_1[,4],sep='')
  li_1_sum[,5] <- paste(li_1[,1],'+',li_1[,2],'+',li_1[,3],'+',li_1[,4],'+',li_1[,5],sep='')
  li_1_sum[,6] <- paste(li_1[,1],'+',li_1[,2],'+',li_1[,3],'+',li_1[,4],'+',li_1[,5],'+',li_1[,6],sep='')
  
  Reparm<-array(rep(0,n_items*nclass*(scale.num)),dim = c(n_items,nclass,(scale.num))) # placeholder for the loop results 
  
  # Fill in the PImat - a 3D object, n_items x nclass x nstep  
  for (loops in 1:nstep){
    for(loopi in 1:n_items){
      for(loopc in 1:nclass){
        Reparm[loopi,loopc,1]<-paste('  PImat[',loopi,',',loopc,'][1]=0;\n',sep='')
        Reparm[loopi,loopc,loops+1]<-paste('  PImat[',loopi,',',loopc,'][',loops+1,']=', li_0_sum[loopi,loops],
                                           '+(', li_1_sum[loopi,loops], ')*', PfbyI[loopi,loopc], 
                                           ';\n', sep='') 
      }
    }   
  }
  
  # build a Frankenstein 
  Modelcontainer<-paste('vector[Nc] contributionsC;\n','    
                        vector[Ni] contributionsI;\n\n',sep='')
  Parmprior<-paste(c(paste('   //Prior\n'),
                     paste( li_0,'~normal(0,2)',';\n'),
                     paste( li_1,'~normal(0,2)', ';\n'),
                     paste('Vc~dirichlet(rep_vector(2.0, Nc));',sep='')))
  
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
                     '}\n')
                   ,collapse='')
  
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



Q=matrix(c(rep(c(1,0,0,0),10),
           rep(c(0,1,0,0),10),
           rep(c(0,0,1,0),10),
           rep(c(0,0,0,1),10)),
           40,4, byrow=T)

NRDM(Q,7)



