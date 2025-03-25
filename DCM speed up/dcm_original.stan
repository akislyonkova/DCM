
data{
  int Np;
  int Ni;
  int Nc;
  int Ns;
  int Y[Np, Ni];
}
parameters{
  simplex[Nc] Vc;
  real l1I ;
  real l2I ;
  real l3I ;
  
  real<lower=0> l1M ;
  real<lower=0> l2M ;
  real<lower=0> l3M ;
  
  real<lower=0> d1 ;
  real<lower=0> d2 ;
  real<lower=0> d3 ;
  
}
transformed parameters{
  vector[Ns] PImat[Ni, Nc];
  PImat[1,1][1]=0;
  PImat[2,1][1]=0;
  PImat[3,1][1]=0;
  
  PImat[1,2][1]=0;
  PImat[2,2][1]=0;
  PImat[3,2][1]=0;
  
  PImat[1,3][1]=0;
  PImat[2,3][1]=0;
  PImat[3,3][1]=0;
  
  PImat[1,4][1]=0;
  PImat[2,4][1]=0;
  PImat[3,4][1]=0;
  
  PImat[1,1][2]=1*(l1I+l1M*0)+(4-1)*d1;
  PImat[2,1][2]=1*(l2I+l2M*0)+(4-1)*d2;
  PImat[3,1][2]=1*(l3I+l3M*0)+(4-1)*d3;
  
  PImat[1,2][2]=1*(l1I+l1M*1)+(4-1)*d1;
  PImat[2,2][2]=1*(l2I+l2M*1)+(4-1)*d2;
  PImat[3,2][2]=1*(l3I+l3M*1)+(4-1)*d3;
  
  PImat[1,3][2]=1*(l1I+l1M*0)+(4-1)*d1;
  PImat[2,3][2]=1*(l2I+l2M*0)+(4-1)*d2;
  PImat[3,3][2]=1*(l3I+l3M*0)+(4-1)*d3;
  
  PImat[1,4][2]=1*(l1I+l1M*1)+(4-1)*d1;
  PImat[2,4][2]=1*(l2I+l2M*1)+(4-1)*d2;
  PImat[3,4][2]=1*(l3I+l3M*1)+(4-1)*d3;
  
  PImat[1,1][3]=2*(l1I+l1M*0)+(4-2)*d1;
  PImat[2,1][3]=2*(l2I+l2M*0)+(4-2)*d2;
  PImat[3,1][3]=2*(l3I+l3M*0)+(4-2)*d3;
  
  PImat[1,2][3]=2*(l1I+l1M*1)+(4-2)*d1;
  PImat[2,2][3]=2*(l2I+l2M*1)+(4-2)*d2;
  PImat[3,2][3]=2*(l3I+l3M*1)+(4-2)*d3;
  
  PImat[1,3][3]=2*(l1I+l1M*0)+(4-2)*d1;
  PImat[2,3][3]=2*(l2I+l2M*0)+(4-2)*d2;
  PImat[3,3][3]=2*(l3I+l3M*0)+(4-2)*d3;
  
  PImat[1,4][3]=2*(l1I+l1M*1)+(4-2)*d1;
  PImat[2,4][3]=2*(l2I+l2M*1)+(4-2)*d2;
  PImat[3,4][3]=2*(l3I+l3M*1)+(4-2)*d3;
  
  PImat[1,1][4]=3*(l1I+l1M*0)+(4-3)*d1;
  PImat[2,1][4]=3*(l2I+l2M*0)+(4-3)*d2;
  PImat[3,1][4]=3*(l3I+l3M*0)+(4-3)*d3;
  
  PImat[1,2][4]=3*(l1I+l1M*1)+(4-3)*d1;
  PImat[2,2][4]=3*(l2I+l2M*1)+(4-3)*d2;
  PImat[3,2][4]=3*(l3I+l3M*1)+(4-3)*d3;
  
  PImat[1,3][4]=3*(l1I+l1M*0)+(4-3)*d1;
  PImat[2,3][4]=3*(l2I+l2M*0)+(4-3)*d2;
  PImat[3,3][4]=3*(l3I+l3M*0)+(4-3)*d3;
  
  
  PImat[1,4][4]=3*(l1I+l1M*1)+(4-3)*d1;
  PImat[2,4][4]=3*(l2I+l2M*1)+(4-3)*d2;
  PImat[3,4][4]=3*(l3I+l3M*1)+(4-3)*d3;
  
  
}

model {
  vector[Nc] contributionsC;
  vector[Ni] contributionsI;
  
  //Prior
  l1I ~normal(0,2) ;
  l2I ~normal(0,2) ;
  l3I ~normal(0,2) ;
  
  l1M ~normal(0,2) ;
  l2M ~normal(0,2) ;
  l3M ~normal(0,2) ;
  
  
  d1 ~normal(0,2) ;
  d2 ~normal(0,2) ;
  d3 ~normal(0,2) ;
  
  Vc~dirichlet(rep_vector(2.0, Nc)); 
  
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
}




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
