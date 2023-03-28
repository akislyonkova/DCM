
data{
  int Np;
  int Ni;
  int Nc;
  int Ns;
  int Nstep;
  int Y[Np, Ni];
  int W[Ni, Nc];
}

parameters{
  simplex[Nc] Vc;
  vector <lower=0> [Ni] li_1;
  vector [Ni] li_0;
  matrix <lower=0> [Nc, Nstep] ls_0;
  matrix <lower=0> [Nc, Nstep] ls_1;
}

transformed parameters{
  vector[Ns] PImat[Ni, Nc];
  
  vector[Nc] ls_0_col;
  matrix[Nstep, Nc] ls_0_cumul_t;
  vector[Nc] ls_1_col;
  matrix[Nstep, Nc] ls_1_cumul_t;
  real Wmat;
  
  for (item in 1:Ni){
    for (pf in 1:Nc){
      PImat[item, pf, 1] = 0;
    }
  }
  
  
  ls_0_cumul_t = ls_0';
  ls_1_cumul_t = ls_1';
  
  for (step in 1:Nstep){
    ls_0_col = col(ls_0, step);
    ls_0_cumul_t[step] = cumulative_sum(ls_0_col)';
    ls_1_col = col(ls_1, step);
    ls_1_cumul_t[step] = cumulative_sum(ls_1_col)';
  } 
  
  
  for (step in 1:Nstep) {
    for (item in 1:Ni) {
      for (pf in 1:Nc){
        Wmat = W[item,pf];
        PImat[item, pf, step+1] = (li_0[item] + li_1[item]*Wmat) - ls_0_cumul_t[step,pf] + ls_1_cumul_t[step,pf]*Wmat;
      }
      
    }
  }
  
}

model {
  vector[Nc] contributionsC;
  vector[Ni] contributionsI;
  
  //Prior
  li_1 ~ normal(0,20);
  li_0 ~ normal(0,20);
  ls_0 [Nc, Nstep] ~ normal(0,20);
  ls_1 [Nc, Nstep] ~ normal(0,20);
  
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
  matrix[Np,Nc] contributionsPC;
  vector[Ni] contributionsI;
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
