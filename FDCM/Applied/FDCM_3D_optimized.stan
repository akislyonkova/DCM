

functions {
  array[] matrix item_logprob(vector l0, vector lam, vector tau, matrix A,
                              array[] int att, row_vector mc, row_vector tc) {
    int Ni = num_elements(l0);
    int Nc = rows(A);
    int Ns = num_elements(mc);
    array[Ni] matrix[Nc, Ns] LP;
    for (i in 1:Ni) {
      vector[Nc] x = l0[i] + lam[i] * A[:, att[i]];            
      matrix[Nc, Ns] eta = x * mc + tau[i] * rep_matrix(tc, Nc);
      for (c in 1:Nc) {
        LP[i, c] = eta[c] - log_sum_exp(eta[c]);
      }
    }
    return LP;
  }
}

data {
  int<lower=1> Np;                              
  int<lower=1> Ni;                              
  int<lower=1> Nc;                              
  int<lower=2> Ns;                              
  array[Np, Ni] int<lower=1, upper=Ns> Y;
}

transformed data {
  matrix[Nc, 3] A;
  array[3] int bit = {1, 2, 4};
  for (c in 1:Nc)
    for (k in 1:3)
      A[c, k] = ((c - 1) %/% bit[k]) % 2;


  array[Ni] int att;
  for (i in 1:Ni) att[i] = (i - 1) %/% (Ni %/% 3) + 1;

  row_vector[Ns] mc;
  row_vector[Ns] tc;
  for (s in 1:Ns) {
    mc[s] = s - 1;
    tc[s] = (s == 1) ? 0 : Ns - (s - 1);
  }

  array[Ni, Np] int Yt;
  for (i in 1:Ni)
    for (p in 1:Np) Yt[i, p] = Y[p, i];
}

parameters {
  simplex[Nc] Vc;
  vector[Ni] l0;
  vector<lower=0>[Ni] lam;
  vector<lower=0>[Ni] tau;
}

model {
  l0  ~ normal(0, 2);
  lam ~ normal(0, 2);
  tau ~ normal(0, 2);
  Vc  ~ dirichlet(rep_vector(2.0, Nc));

  
  array[Ni] matrix[Nc, Ns] LP = item_logprob(l0, lam, tau, A, att, mc, tc);
  matrix[Nc, Np] ll = rep_matrix(log(Vc), Np);   
  for (i in 1:Ni)
    ll += LP[i][:, Yt[i]];
  for (p in 1:Np)
    target += log_sum_exp(col(ll, p));
}

generated quantities {
  matrix[Np, Nc] contributionsPC;   
  vector[Np] log_lik;               
  {
    array[Ni] matrix[Nc, Ns] LP = item_logprob(l0, lam, tau, A, att, mc, tc);
    matrix[Nc, Np] ll = rep_matrix(0, Nc, Np);
    for (i in 1:Ni)
      ll += LP[i][:, Yt[i]];
    for (p in 1:Np) {
      contributionsPC[p] = exp(col(ll, p))';
      log_lik[p] = log_sum_exp(log(Vc) + col(ll, p));
    }
  }
}
