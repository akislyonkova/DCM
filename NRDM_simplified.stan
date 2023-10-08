data{
  int Np;
  int Ni;
  int Nc;
  int Ns;
  int Nstep;
  int Y[Np, Ni];
}

parameters{
  simplex[Nc] Vc;
  matrix [Ni, Nstep] li_0;
  matrix [Ni, Nstep] li_1;
}

transformed parameters{
  vector[Ns] PImat[Ni, Nc];
  
  vector[Ni] li_0_col;
  matrix[Nstep, Ni] li_0_cumul_t;
  vector[Ni] li_1_col;
  matrix[Nstep, Ni] li_1_cumul_t;
  
  for (step in 1:Nstep) {
    li_0_col = col(li_0, step);
    li_0_cumul_t[step] = cumulative_sum(li_0_col)';
    li_1_col = col(li_1, step);
    li_1_cumul_t[step] = cumulative_sum(li_1_col)';
    for (item in 1:Ni) {
      for (pf in 1:Nc){
        PImat[item, pf, 1] = 0;
        PImat[item, pf, step+1] = li_0_cumul_t[step,item]+li_1_cumul_t[step,item];
      }
    }
  }
} 

model {
  vector[Nc] contributionsC;
  vector[Ni] contributionsI;
  
  //Prior
  li_0 [Ni, Nstep]~normal(0,20);
  li_1 [Ni, Nstep]~normal(0,20);
  
  Vc~dirichlet(rep_vector(2.0, Nc));
  
  ////Likelihood
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
 