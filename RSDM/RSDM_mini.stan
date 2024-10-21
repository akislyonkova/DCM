
data{
  int Np;
  int Ni;
  int Nc;
  int Ns;
  int Y[Np, Ni];
}
parameters{
  simplex[Nc] Vc;
  real<lower=0> l1_1 ;
  real<lower=0> l2_1 ;
  real<lower=0> l3_1 ;
  real<lower=0> l4_1 ;
  real<lower=0> l5_1 ;
  real<lower=0> l6_1 ;
  real<lower=0> l7_1 ;
  real<lower=0> l8_1 ;
  real l1_0 ;
  real l2_0 ;
  real l3_0 ;
  real l4_0 ;
  real l5_0 ;
  real l6_0 ;
  real l7_0 ;
  real l8_0 ;
  real<lower=0> step1D1I;
  real<lower=0> step2D1I;
  real<lower=0> step3D1I;
  real<lower=0> step4D1I;
  real<lower=0> step1D2I;
  real<lower=0> step2D2I;
  real<lower=0> step3D2I;
  real<lower=0> step4D2I;
  real<lower=0> step1D1M;
  real<lower=0> step2D1M;
  real<lower=0> step3D1M;
  real<lower=0> step4D1M;
  real<lower=0> step1D2M;
  real<lower=0> step2D2M;
  real<lower=0> step3D2M;
  real<lower=0> step4D2M;
}
transformed parameters{
  vector[Ns] PImat[Ni, Nc];
  PImat[1,1,1]=0;
  PImat[2,1,1]=0;
  PImat[3,1,1]=0;
  PImat[4,1,1]=0;
  PImat[5,1,1]=0;
  PImat[6,1,1]=0;
  PImat[7,1,1]=0;
  PImat[8,1,1]=0;
  
  PImat[1,2,1]=0;
  PImat[2,2,1]=0;
  PImat[3,2,1]=0;
  PImat[4,2,1]=0;
  PImat[5,2,1]=0;
  PImat[6,2,1]=0;
  PImat[7,2,1]=0;
  PImat[8,2,1]=0;
  
  PImat[1,3,1]=0;
  PImat[2,3,1]=0;
  PImat[3,3,1]=0;
  PImat[4,3,1]=0;
  PImat[5,3,1]=0;
  PImat[6,3,1]=0;
  PImat[7,3,1]=0;
  PImat[8,3,1]=0;
  
  PImat[1,4,1]=0;
  PImat[2,4,1]=0;
  PImat[3,4,1]=0;
  PImat[4,4,1]=0;
  PImat[5,4,1]=0;
  PImat[6,4,1]=0;
  PImat[7,4,1]=0;
  PImat[8,4,1]=0;
  
  PImat[1,1,2]=(l1_0)-step1D1I;
  PImat[2,1,2]=(l2_0)-step1D1I;
  PImat[3,1,2]=(l3_0)-step1D1I;
  PImat[4,1,2]=(l4_0)-step1D1I;
  PImat[5,1,2]=(l5_0)-step1D2I;
  PImat[6,1,2]=(l6_0)-step1D2I;
  PImat[7,1,2]=(l7_0)-step1D2I;
  PImat[8,1,2]=(l8_0)-step1D2I;
  
  PImat[1,1,3]=(l1_0)-step1D1I-step2D1I;
  PImat[2,1,3]=(l2_0)-step1D1I-step2D1I;
  PImat[3,1,3]=(l3_0)-step1D1I-step2D1I;
  PImat[4,1,3]=(l4_0)-step1D1I-step2D1I;
  PImat[5,1,3]=(l5_0)-step1D2I-step2D2I;
  PImat[6,1,3]=(l6_0)-step1D2I-step2D2I;
  PImat[7,1,3]=(l7_0)-step1D2I-step2D2I;
  PImat[8,1,3]=(l8_0)-step1D2I-step2D2I;
  
  PImat[1,1,4]=(l1_0)-step1D1I-step2D1I-step3D1I;
  PImat[2,1,4]=(l2_0)-step1D1I-step2D1I-step3D1I;
  PImat[3,1,4]=(l3_0)-step1D1I-step2D1I-step3D1I;
  PImat[4,1,4]=(l4_0)-step1D1I-step2D1I-step3D1I;
  PImat[5,1,4]=(l5_0)-step1D2I-step2D2I-step3D2I;
  PImat[6,1,4]=(l6_0)-step1D2I-step2D2I-step3D2I;
  PImat[7,1,4]=(l7_0)-step1D2I-step2D2I-step3D2I;
  PImat[8,1,4]=(l8_0)-step1D2I-step2D2I-step3D2I;
  
  PImat[1,1,5]=(l1_0)-step1D1I-step2D1I-step3D1I-step4D1I;
  PImat[2,1,5]=(l2_0)-step1D1I-step2D1I-step3D1I-step4D1I;
  PImat[3,1,5]=(l3_0)-step1D1I-step2D1I-step3D1I-step4D1I;
  PImat[4,1,5]=(l4_0)-step1D1I-step2D1I-step3D1I-step4D1I;
  PImat[5,1,5]=(l5_0)-step1D2I-step2D2I-step3D2I-step4D2I;
  PImat[6,1,5]=(l6_0)-step1D2I-step2D2I-step3D2I-step4D2I;
  PImat[7,1,5]=(l7_0)-step1D2I-step2D2I-step3D2I-step4D2I;
  PImat[8,1,5]=(l8_0)-step1D2I-step2D2I-step3D2I-step4D2I;
  
  
  PImat[1,2,2]=(l1_0+l1_1)-step1D1I+step1D1M;
  PImat[2,2,2]=(l2_0+l2_1)-step1D1I+step1D1M;
  PImat[3,2,2]=(l3_0+l3_1)-step1D1I+step1D1M;
  PImat[4,2,2]=(l4_0+l4_1)-step1D1I+step1D1M;
  PImat[5,2,2]=(l5_0)-step1D2I;
  PImat[6,2,2]=(l6_0)-step1D2I;
  PImat[7,2,2]=(l7_0)-step1D2I;
  PImat[8,2,2]=(l8_0)-step1D2I;
  
  
  PImat[1,2,3]=(l1_0+l1_1)-step1D1I-step2D1I+step1D1M+step2D1M;
  PImat[2,2,3]=(l2_0+l2_1)-step1D1I-step2D1I+step1D1M+step2D1M;
  PImat[3,2,3]=(l3_0+l3_1)-step1D1I-step2D1I+step1D1M+step2D1M;
  PImat[4,2,3]=(l4_0+l4_1)-step1D1I-step2D1I+step1D1M+step2D1M;
  PImat[5,2,3]=(l5_0)-step1D2I-step2D2I;
  PImat[6,2,3]=(l6_0)-step1D2I-step2D2I;
  PImat[7,2,3]=(l7_0)-step1D2I-step2D2I;
  PImat[8,2,3]=(l8_0)-step1D2I-step2D2I;
  
  
  PImat[1,2,4]=(l1_0+l1_1)-step1D1I-step2D1I-step3D1I+step1D1M+step2D1M+step3D1M;
  PImat[2,2,4]=(l2_0+l2_1)-step1D1I-step2D1I-step3D1I+step1D1M+step2D1M+step3D1M;
  PImat[3,2,4]=(l3_0+l3_1)-step1D1I-step2D1I-step3D1I+step1D1M+step2D1M+step3D1M;
  PImat[4,2,4]=(l4_0+l4_1)-step1D1I-step2D1I-step3D1I+step1D1M+step2D1M+step3D1M;
  PImat[5,2,4]=(l5_0)-step1D2I-step2D2I-step3D2I;
  PImat[6,2,4]=(l6_0)-step1D2I-step2D2I-step3D2I;
  PImat[7,2,4]=(l7_0)-step1D2I-step2D2I-step3D2I;
  PImat[8,2,4]=(l8_0)-step1D2I-step2D2I-step3D2I;
  
  PImat[1,2,5]=(l1_0+l1_1)-step1D1I-step2D1I-step3D1I+step1D1M+step2D1M+step3D1M-step4D1I+step4D1M;
  PImat[2,2,5]=(l2_0+l2_1)-step1D1I-step2D1I-step3D1I+step1D1M+step2D1M+step3D1M-step4D1I+step4D1M;
  PImat[3,2,5]=(l3_0+l3_1)-step1D1I-step2D1I-step3D1I+step1D1M+step2D1M+step3D1M-step4D1I+step4D1M;
  PImat[4,2,5]=(l4_0+l4_1)-step1D1I-step2D1I-step3D1I+step1D1M+step2D1M+step3D1M-step4D1I+step4D1M;
  PImat[5,2,5]=(l5_0)-step1D2I-step2D2I-step3D2I-step4D2I;
  PImat[6,2,5]=(l6_0)-step1D2I-step2D2I-step3D2I-step4D2I;
  PImat[7,2,5]=(l7_0)-step1D2I-step2D2I-step3D2I-step4D2I;
  PImat[8,2,5]=(l8_0)-step1D2I-step2D2I-step3D2I-step4D2I;
  
  
  PImat[1,3,2]=(l1_0)-step1D1I;
  PImat[2,3,2]=(l2_0)-step1D1I;
  PImat[3,3,2]=(l3_0)-step1D1I;
  PImat[4,3,2]=(l4_0)-step1D1I;
  PImat[5,3,2]=(l5_0+l5_1)-step1D2I+step1D2M;
  PImat[6,3,2]=(l6_0+l6_1)-step1D2I+step1D2M;
  PImat[7,3,2]=(l7_0+l7_1)-step1D2I+step1D2M;
  PImat[8,3,2]=(l8_0+l8_1)-step1D2I+step1D2M;
  
  PImat[1,3,3]=(l1_0)-step1D1I-step2D1I;
  PImat[2,3,3]=(l2_0)-step1D1I-step2D1I;
  PImat[3,3,3]=(l3_0)-step1D1I-step2D1I;
  PImat[4,3,3]=(l4_0)-step1D1I-step2D1I;
  PImat[5,3,3]=(l5_0+l5_1)-step1D2I+step1D2M-step2D2I+step2D2M;
  PImat[6,3,3]=(l6_0+l6_1)-step1D2I+step1D2M-step2D2I+step2D2M;
  PImat[7,3,3]=(l7_0+l7_1)-step1D2I+step1D2M-step2D2I+step2D2M;
  PImat[8,3,3]=(l8_0+l8_1)-step1D2I+step1D2M-step2D2I+step2D2M;
  
  PImat[1,3,4]=(l1_0)-step1D1I-step2D1I-step3D1I;
  PImat[2,3,4]=(l2_0)-step1D1I-step2D1I-step3D1I;
  PImat[3,3,4]=(l3_0)-step1D1I-step2D1I-step3D1I;
  PImat[4,3,4]=(l4_0)-step1D1I-step2D1I-step3D1I;
  PImat[5,3,4]=(l5_0+l5_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M;
  PImat[6,3,4]=(l6_0+l6_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M;
  PImat[7,3,4]=(l7_0+l7_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M;
  PImat[8,3,4]=(l8_0+l8_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M;
  
  PImat[1,3,5]=(l1_0)-step1D1I-step2D1I-step3D1I-step4D1I;
  PImat[2,3,5]=(l2_0)-step1D1I-step2D1I-step3D1I-step4D1I;
  PImat[3,3,5]=(l3_0)-step1D1I-step2D1I-step3D1I-step4D1I;
  PImat[4,3,5]=(l4_0)-step1D1I-step2D1I-step3D1I-step4D1I;
  PImat[5,3,5]=(l5_0+l5_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M-step4D2I+step4D2M;
  PImat[6,3,5]=(l6_0+l6_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M-step4D2I+step4D2M;
  PImat[7,3,5]=(l7_0+l7_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M-step4D2I+step4D2M;
  PImat[8,3,5]=(l8_0+l8_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M-step4D2I+step4D2M;
  
  PImat[1,4,2]=(l1_0+l1_1)-step1D1I+step1D1M;
  PImat[2,4,2]=(l2_0+l2_1)-step1D1I+step1D1M;
  PImat[3,4,2]=(l3_0+l3_1)-step1D1I+step1D1M;
  PImat[4,4,2]=(l4_0+l4_1)-step1D1I+step1D1M;
  PImat[5,4,2]=(l5_0+l5_1)-step1D2I+step1D2M;
  PImat[6,4,2]=(l6_0+l6_1)-step1D2I+step1D2M;
  PImat[7,4,2]=(l7_0+l7_1)-step1D2I+step1D2M;
  PImat[8,4,2]=(l8_0+l8_1)-step1D2I+step1D2M;
  
  PImat[1,4,3]=(l1_0+l1_1)-step1D1I+step1D1M-step2D1I+step2D1M;
  PImat[2,4,3]=(l2_0+l2_1)-step1D1I+step1D1M-step2D1I+step2D1M;
  PImat[3,4,3]=(l3_0+l3_1)-step1D1I+step1D1M-step2D1I+step2D1M;
  PImat[4,4,3]=(l4_0+l4_1)-step1D1I+step1D1M-step2D1I+step2D1M;
  PImat[5,4,3]=(l5_0+l5_1)-step1D2I+step1D2M-step2D2I+step2D2M;
  PImat[6,4,3]=(l6_0+l6_1)-step1D2I+step1D2M-step2D2I+step2D2M;
  PImat[7,4,3]=(l7_0+l7_1)-step1D2I+step1D2M-step2D2I+step2D2M;
  PImat[8,4,3]=(l8_0+l8_1)-step1D2I+step1D2M-step2D2I+step2D2M;
  
  
  PImat[1,4,4]=(l1_0+l1_1)-step1D1I+step1D1M-step2D1I+step2D1M-step3D1I+step3D1M;
  PImat[2,4,4]=(l2_0+l2_1)-step1D1I+step1D1M-step2D1I+step2D1M-step3D1I+step3D1M;
  PImat[3,4,4]=(l3_0+l3_1)-step1D1I+step1D1M-step2D1I+step2D1M-step3D1I+step3D1M;
  PImat[4,4,4]=(l4_0+l4_1)-step1D1I+step1D1M-step2D1I+step2D1M-step3D1I+step3D1M;
  PImat[5,4,4]=(l5_0+l5_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M;
  PImat[6,4,4]=(l6_0+l6_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M;
  PImat[7,4,4]=(l7_0+l7_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M;
  PImat[8,4,4]=(l8_0+l8_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M;
  
  PImat[1,4,5]=(l1_0+l1_1)-step1D1I+step1D1M-step2D1I+step2D1M-step3D1I+step3D1M-step4D1I+step4D1M;
  PImat[2,4,5]=(l2_0+l2_1)-step1D1I+step1D1M-step2D1I+step2D1M-step3D1I+step3D1M-step4D1I+step4D1M;
  PImat[3,4,5]=(l3_0+l3_1)-step1D1I+step1D1M-step2D1I+step2D1M-step3D1I+step3D1M-step4D1I+step4D1M;
  PImat[4,4,5]=(l4_0+l4_1)-step1D1I+step1D1M-step2D1I+step2D1M-step3D1I+step3D1M-step4D1I+step4D1M;
  PImat[5,4,5]=(l5_0+l5_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M-step4D2I+step4D2M;
  PImat[6,4,5]=(l6_0+l6_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M-step4D2I+step4D2M;
  PImat[7,4,5]=(l7_0+l7_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M-step4D2I+step4D2M;
  PImat[8,4,5]=(l8_0+l8_1)-step1D2I+step1D2M-step2D2I+step2D2M-step3D2I+step3D2M-step4D2I+step4D2M;
  
}

model {
  vector[Nc] contributionsC;
  vector[Ni] contributionsI;
  
  //Prior
  l1_1~normal(0,20);
  l2_1~normal(0,20);
  l3_1~normal(0,20);
  l4_1~normal(0,20);
  l5_1~normal(0,20);
  l6_1~normal(0,20);
  l7_1~normal(0,20);
  l8_1~normal(0,20);
  
  l1_0~normal(0,20);
  l2_0~normal(0,20);
  l3_0~normal(0,20);
  l4_0~normal(0,20);
  l5_0~normal(0,20);
  l6_0~normal(0,20);
  l7_0~normal(0,20);
  l8_0~normal(0,20);
  
  step1D1I~normal(0,20);
  step2D1I~normal(0,20);
  step3D1I~normal(0,20);
  step4D1I~normal(0,20);
  step1D2I~normal(0,20);
  step2D2I~normal(0,20);
  step3D2I~normal(0,20);
  step4D2I~normal(0,20);
  step1D1M~normal(0,20);
  step2D1M~normal(0,20);
  step3D1M~normal(0,20);
  step4D1M~normal(0,20);
  step1D2M~normal(0,20);
  step2D2M~normal(0,20);
  step3D2M~normal(0,20);
  step4D2M~normal(0,20);
  Vc~dirichlet(rep_vector(2.0, Nc)); 
  
  
  //Likelihood
  for (iterp in 1:Np){
    for (iterc in 1:Nc){
      for (iteri in 1:Ni){
        contributionsI[iteri]= categorical_lpmf(Y[iterp,iteri]+1| softmax(((PImat[iteri,iterc])))); 
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
        contributionsI[iteri]= categorical_lpmf(Y[iterp,iteri]+1| softmax(((PImat[iteri,iterc]))));
      }
      contributionsPC[iterp,iterc]=prod(exp(contributionsI));
    }
    
  }
  
}
