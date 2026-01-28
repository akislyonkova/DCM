
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
  real l4I ;
  real l5I ;
  real l6I ;
  real l7I ;
  real l8I ;
  real l9I ;
  real l10I ;
  real<lower=0> l1M ;
  real<lower=0> l2M ;
  real<lower=0> l3M ;
  real<lower=0> l4M ;
  real<lower=0> l5M ;
  real<lower=0> l6M ;
  real<lower=0> l7M ;
  real<lower=0> l8M ;
  real<lower=0> l9M ;
  real<lower=0> l10M ;
  real<upper=0> step1_ID1 ;
  real<upper=0> step1_ID2 ;
  real<upper=0> step2_ID1 ;
  real<upper=0> step2_ID2 ;
  real<upper=0> step3_ID1 ;
  real<upper=0> step3_ID2 ;
  real<upper=0> step4_ID1 ;
  real<upper=0> step4_ID2 ;
}
transformed parameters{
  vector[Ns] PImat[Ni, Nc];
  PImat[1,1][1]=0;
  PImat[2,1][1]=0;
  PImat[3,1][1]=0;
  PImat[4,1][1]=0;
  PImat[5,1][1]=0;
  PImat[6,1][1]=0;
  PImat[7,1][1]=0;
  PImat[8,1][1]=0;
  PImat[9,1][1]=0;
  PImat[10,1][1]=0;
  PImat[1,2][1]=0;
  PImat[2,2][1]=0;
  PImat[3,2][1]=0;
  PImat[4,2][1]=0;
  PImat[5,2][1]=0;
  PImat[6,2][1]=0;
  PImat[7,2][1]=0;
  PImat[8,2][1]=0;
  PImat[9,2][1]=0;
  PImat[10,2][1]=0;
  PImat[1,3][1]=0;
  PImat[2,3][1]=0;
  PImat[3,3][1]=0;
  PImat[4,3][1]=0;
  PImat[5,3][1]=0;
  PImat[6,3][1]=0;
  PImat[7,3][1]=0;
  PImat[8,3][1]=0;
  PImat[9,3][1]=0;
  PImat[10,3][1]=0;
  PImat[1,4][1]=0;
  PImat[2,4][1]=0;
  PImat[3,4][1]=0;
  PImat[4,4][1]=0;
  PImat[5,4][1]=0;
  PImat[6,4][1]=0;
  PImat[7,4][1]=0;
  PImat[8,4][1]=0;
  PImat[9,4][1]=0;
  PImat[10,4][1]=0;
  PImat[1,1][2]=1*(l1I+l1M*0)+step1_ID1;
  PImat[2,1][2]=1*(l2I+l2M*0)+step1_ID1;
  PImat[3,1][2]=1*(l3I+l3M*0)+step1_ID1;
  PImat[4,1][2]=1*(l4I+l4M*0)+step1_ID1;
  PImat[5,1][2]=1*(l5I+l5M*0)+step1_ID1;
  PImat[6,1][2]=1*(l6I+l6M*0)+step1_ID2;
  PImat[7,1][2]=1*(l7I+l7M*0)+step1_ID2;
  PImat[8,1][2]=1*(l8I+l8M*0)+step1_ID2;
  PImat[9,1][2]=1*(l9I+l9M*0)+step1_ID2;
  PImat[10,1][2]=1*(l10I+l10M*0)+step1_ID2;
  PImat[1,2][2]=1*(l1I+l1M*1)+step1_ID1;
  PImat[2,2][2]=1*(l2I+l2M*1)+step1_ID1;
  PImat[3,2][2]=1*(l3I+l3M*1)+step1_ID1;
  PImat[4,2][2]=1*(l4I+l4M*1)+step1_ID1;
  PImat[5,2][2]=1*(l5I+l5M*1)+step1_ID1;
  PImat[6,2][2]=1*(l6I+l6M*0)+step1_ID2;
  PImat[7,2][2]=1*(l7I+l7M*0)+step1_ID2;
  PImat[8,2][2]=1*(l8I+l8M*0)+step1_ID2;
  PImat[9,2][2]=1*(l9I+l9M*0)+step1_ID2;
  PImat[10,2][2]=1*(l10I+l10M*0)+step1_ID2;
  PImat[1,3][2]=1*(l1I+l1M*0)+step1_ID1;
  PImat[2,3][2]=1*(l2I+l2M*0)+step1_ID1;
  PImat[3,3][2]=1*(l3I+l3M*0)+step1_ID1;
  PImat[4,3][2]=1*(l4I+l4M*0)+step1_ID1;
  PImat[5,3][2]=1*(l5I+l5M*0)+step1_ID1;
  PImat[6,3][2]=1*(l6I+l6M*1)+step1_ID2;
  PImat[7,3][2]=1*(l7I+l7M*1)+step1_ID2;
  PImat[8,3][2]=1*(l8I+l8M*1)+step1_ID2;
  PImat[9,3][2]=1*(l9I+l9M*1)+step1_ID2;
  PImat[10,3][2]=1*(l10I+l10M*1)+step1_ID2;
  PImat[1,4][2]=1*(l1I+l1M*1)+step1_ID1;
  PImat[2,4][2]=1*(l2I+l2M*1)+step1_ID1;
  PImat[3,4][2]=1*(l3I+l3M*1)+step1_ID1;
  PImat[4,4][2]=1*(l4I+l4M*1)+step1_ID1;
  PImat[5,4][2]=1*(l5I+l5M*1)+step1_ID1;
  PImat[6,4][2]=1*(l6I+l6M*1)+step1_ID2;
  PImat[7,4][2]=1*(l7I+l7M*1)+step1_ID2;
  PImat[8,4][2]=1*(l8I+l8M*1)+step1_ID2;
  PImat[9,4][2]=1*(l9I+l9M*1)+step1_ID2;
  PImat[10,4][2]=1*(l10I+l10M*1)+step1_ID2;
  PImat[1,1][3]=2*(l1I+l1M*0)+step1_ID1+step2_ID1;
  PImat[2,1][3]=2*(l2I+l2M*0)+step1_ID1+step2_ID1;
  PImat[3,1][3]=2*(l3I+l3M*0)+step1_ID1+step2_ID1;
  PImat[4,1][3]=2*(l4I+l4M*0)+step1_ID1+step2_ID1;
  PImat[5,1][3]=2*(l5I+l5M*0)+step1_ID1+step2_ID1;
  PImat[6,1][3]=2*(l6I+l6M*0)+step1_ID2+step2_ID2;
  PImat[7,1][3]=2*(l7I+l7M*0)+step1_ID2+step2_ID2;
  PImat[8,1][3]=2*(l8I+l8M*0)+step1_ID2+step2_ID2;
  PImat[9,1][3]=2*(l9I+l9M*0)+step1_ID2+step2_ID2;
  PImat[10,1][3]=2*(l10I+l10M*0)+step1_ID2+step2_ID2;
  PImat[1,2][3]=2*(l1I+l1M*1)+step1_ID1+step2_ID1;
  PImat[2,2][3]=2*(l2I+l2M*1)+step1_ID1+step2_ID1;
  PImat[3,2][3]=2*(l3I+l3M*1)+step1_ID1+step2_ID1;
  PImat[4,2][3]=2*(l4I+l4M*1)+step1_ID1+step2_ID1;
  PImat[5,2][3]=2*(l5I+l5M*1)+step1_ID1+step2_ID1;
  PImat[6,2][3]=2*(l6I+l6M*0)+step1_ID2+step2_ID2;
  PImat[7,2][3]=2*(l7I+l7M*0)+step1_ID2+step2_ID2;
  PImat[8,2][3]=2*(l8I+l8M*0)+step1_ID2+step2_ID2;
  PImat[9,2][3]=2*(l9I+l9M*0)+step1_ID2+step2_ID2;
  PImat[10,2][3]=2*(l10I+l10M*0)+step1_ID2+step2_ID2;
  PImat[1,3][3]=2*(l1I+l1M*0)+step1_ID1+step2_ID1;
  PImat[2,3][3]=2*(l2I+l2M*0)+step1_ID1+step2_ID1;
  PImat[3,3][3]=2*(l3I+l3M*0)+step1_ID1+step2_ID1;
  PImat[4,3][3]=2*(l4I+l4M*0)+step1_ID1+step2_ID1;
  PImat[5,3][3]=2*(l5I+l5M*0)+step1_ID1+step2_ID1;
  PImat[6,3][3]=2*(l6I+l6M*1)+step1_ID2+step2_ID2;
  PImat[7,3][3]=2*(l7I+l7M*1)+step1_ID2+step2_ID2;
  PImat[8,3][3]=2*(l8I+l8M*1)+step1_ID2+step2_ID2;
  PImat[9,3][3]=2*(l9I+l9M*1)+step1_ID2+step2_ID2;
  PImat[10,3][3]=2*(l10I+l10M*1)+step1_ID2+step2_ID2;
  PImat[1,4][3]=2*(l1I+l1M*1)+step1_ID1+step2_ID1;
  PImat[2,4][3]=2*(l2I+l2M*1)+step1_ID1+step2_ID1;
  PImat[3,4][3]=2*(l3I+l3M*1)+step1_ID1+step2_ID1;
  PImat[4,4][3]=2*(l4I+l4M*1)+step1_ID1+step2_ID1;
  PImat[5,4][3]=2*(l5I+l5M*1)+step1_ID1+step2_ID1;
  PImat[6,4][3]=2*(l6I+l6M*1)+step1_ID2+step2_ID2;
  PImat[7,4][3]=2*(l7I+l7M*1)+step1_ID2+step2_ID2;
  PImat[8,4][3]=2*(l8I+l8M*1)+step1_ID2+step2_ID2;
  PImat[9,4][3]=2*(l9I+l9M*1)+step1_ID2+step2_ID2;
  PImat[10,4][3]=2*(l10I+l10M*1)+step1_ID2+step2_ID2;
  PImat[1,1][4]=3*(l1I+l1M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[2,1][4]=3*(l2I+l2M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[3,1][4]=3*(l3I+l3M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[4,1][4]=3*(l4I+l4M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[5,1][4]=3*(l5I+l5M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[6,1][4]=3*(l6I+l6M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[7,1][4]=3*(l7I+l7M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[8,1][4]=3*(l8I+l8M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[9,1][4]=3*(l9I+l9M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[10,1][4]=3*(l10I+l10M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[1,2][4]=3*(l1I+l1M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[2,2][4]=3*(l2I+l2M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[3,2][4]=3*(l3I+l3M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[4,2][4]=3*(l4I+l4M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[5,2][4]=3*(l5I+l5M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[6,2][4]=3*(l6I+l6M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[7,2][4]=3*(l7I+l7M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[8,2][4]=3*(l8I+l8M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[9,2][4]=3*(l9I+l9M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[10,2][4]=3*(l10I+l10M*0)+step1_ID2+step2_ID2+step3_ID2;
  PImat[1,3][4]=3*(l1I+l1M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[2,3][4]=3*(l2I+l2M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[3,3][4]=3*(l3I+l3M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[4,3][4]=3*(l4I+l4M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[5,3][4]=3*(l5I+l5M*0)+step1_ID1+step2_ID1+step3_ID1;
  PImat[6,3][4]=3*(l6I+l6M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[7,3][4]=3*(l7I+l7M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[8,3][4]=3*(l8I+l8M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[9,3][4]=3*(l9I+l9M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[10,3][4]=3*(l10I+l10M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[1,4][4]=3*(l1I+l1M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[2,4][4]=3*(l2I+l2M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[3,4][4]=3*(l3I+l3M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[4,4][4]=3*(l4I+l4M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[5,4][4]=3*(l5I+l5M*1)+step1_ID1+step2_ID1+step3_ID1;
  PImat[6,4][4]=3*(l6I+l6M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[7,4][4]=3*(l7I+l7M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[8,4][4]=3*(l8I+l8M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[9,4][4]=3*(l9I+l9M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[10,4][4]=3*(l10I+l10M*1)+step1_ID2+step2_ID2+step3_ID2;
  PImat[1,1][5]=4*(l1I+l1M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[2,1][5]=4*(l2I+l2M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[3,1][5]=4*(l3I+l3M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[4,1][5]=4*(l4I+l4M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[5,1][5]=4*(l5I+l5M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[6,1][5]=4*(l6I+l6M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[7,1][5]=4*(l7I+l7M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[8,1][5]=4*(l8I+l8M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[9,1][5]=4*(l9I+l9M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[10,1][5]=4*(l10I+l10M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[1,2][5]=4*(l1I+l1M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[2,2][5]=4*(l2I+l2M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[3,2][5]=4*(l3I+l3M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[4,2][5]=4*(l4I+l4M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[5,2][5]=4*(l5I+l5M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[6,2][5]=4*(l6I+l6M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[7,2][5]=4*(l7I+l7M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[8,2][5]=4*(l8I+l8M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[9,2][5]=4*(l9I+l9M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[10,2][5]=4*(l10I+l10M*0)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[1,3][5]=4*(l1I+l1M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[2,3][5]=4*(l2I+l2M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[3,3][5]=4*(l3I+l3M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[4,3][5]=4*(l4I+l4M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[5,3][5]=4*(l5I+l5M*0)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[6,3][5]=4*(l6I+l6M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[7,3][5]=4*(l7I+l7M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[8,3][5]=4*(l8I+l8M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[9,3][5]=4*(l9I+l9M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[10,3][5]=4*(l10I+l10M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[1,4][5]=4*(l1I+l1M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[2,4][5]=4*(l2I+l2M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[3,4][5]=4*(l3I+l3M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[4,4][5]=4*(l4I+l4M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[5,4][5]=4*(l5I+l5M*1)+step1_ID1+step2_ID1+step3_ID1+step4_ID1;
  PImat[6,4][5]=4*(l6I+l6M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[7,4][5]=4*(l7I+l7M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[8,4][5]=4*(l8I+l8M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[9,4][5]=4*(l9I+l9M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
  PImat[10,4][5]=4*(l10I+l10M*1)+step1_ID2+step2_ID2+step3_ID2+step4_ID2;
}

model {
  vector[Nc] contributionsC;
  vector[Ni] contributionsI;
  
  //Prior
  l1I~normal(0,2);
  l2I~normal(0,2);
  l3I~normal(0,2);
  l4I~normal(0,2);
  l5I~normal(0,2);
  l6I~normal(0,2);
  l7I~normal(0,2);
  l8I~normal(0,2);
  l9I~normal(0,2);
  l10I~normal(0,2);
  l1M~normal(0,2);
  l2M~normal(0,2);
  l3M~normal(0,2);
  l4M~normal(0,2);
  l5M~normal(0,2);
  l6M~normal(0,2);
  l7M~normal(0,2);
  l8M~normal(0,2);
  l9M~normal(0,2);
  l10M~normal(0,2);
  step1_ID1~normal(0,2);
  step1_ID2~normal(0,2);
  step2_ID1~normal(0,2);
  step2_ID2~normal(0,2);
  step3_ID1~normal(0,2);
  step3_ID2~normal(0,2);
  step4_ID1~normal(0,2);
  step4_ID2~normal(0,2);
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

