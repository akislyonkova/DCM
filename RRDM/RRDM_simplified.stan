data{
  int Np;
  int Ni;
  int Nc;
  int Ns;
  int Nr;
  int Y[Np, Ni];
  int W[Ni, Nc];
}
  
parameters{
  simplex[Nc] Vc;
    vector <lower=0> [Ni] li_1;
    vector [Ni] li_0;
    vector <lower=0> [Nr] ls_0;
}
 
transformed parameters{
  vector[Ns] PImat[Ni, Nc];
  
  real current_sum;
  real new_sum;
  real Wmat;
  
  current_sum = 0;
  
  for (step in 1:Nr) {
    new_sum = ls_0[step];
    current_sum = current_sum + new_sum;
    for (item in 1:Ni) {
      for (pf in 1:Nc){
        PImat[item, pf, 1] = 0;
        Wmat = W[item,pf];
        PImat[item, pf, step+1] = (step*(li_0[item] + li_1[item]*Wmat) - current_sum);
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
    ls_0 ~ normal(0,20);
    
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
