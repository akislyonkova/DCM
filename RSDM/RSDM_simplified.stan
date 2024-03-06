data{
  int Np;           // number of participants 
  int Ni;           // number of items 
  int Nc;           // number of profiles 
  int Ns;           // number of response options 
  int Nstep;        // number of steps 
  int Y[Np, Ni];    // response matrix 
  int W[Ni, Nc];    // weight matrix (which item measures which class )
}

parameters{
  simplex[Nc] Vc;
  vector [Ni] li_0;                     // vector of global item intercepts
  vector <lower=0> [Ni] li_1;           // vector of global item main effects 
  matrix <lower=0> [Nc, Nstep] ls_0;    // matrix of step intercepts
  matrix <lower=0> [Nc, Nstep] ls_1;    // matrix of step main effects
}

transformed parameters{
  vector[Ns] PImat[Ni, Nc];
  
  vector[Nc] ls_0_col;
  matrix[Nstep, Nc] ls_0_cumul_t;
  vector[Nc] ls_1_col;
  matrix[Nstep, Nc] ls_1_cumul_t;
  real Wmat;
  
  ls_0_cumul_t = ls_0';
  ls_1_cumul_t = ls_1';
  
  for (step in 1:Nstep) {
    ls_0_col = col(ls_0, step);
    ls_0_cumul_t[step] = cumulative_sum(ls_0_col)';
    ls_1_col = col(ls_1, step);
    ls_1_cumul_t[step] = cumulative_sum(ls_1_col)';
    for (item in 1:Ni) {
      for (pf in 1:Nc){
        PImat[item, pf, 1] = 0;
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
  li_1 ~ normal(0,2);
  li_0 ~ normal(0,2);
  ls_0 [Nc, Nstep] ~ normal(0,2);
  ls_1 [Nc, Nstep] ~ normal(0,2);
  
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
  matrix[Np,Nc] log_lik;
  vector[Ni] contributionsI;
  
  //Posterior
  for (iterp in 1:Np){
    for (iterc in 1:Nc){
      for (iteri in 1:Ni){
        contributionsI[iteri]= categorical_lpmf(Y[iterp,iteri]| softmax(((PImat[iteri,iterc]))));
      }
      contributionsPC[iterp,iterc]=prod(exp(contributionsI));
      log_lik[iterp,iterc]=prod(exp(contributionsI));
    }
  }
}
