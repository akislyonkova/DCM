library('rstan')
library('loo')
options(mc.cores = parallel::detectCores())


# Model fit 

loglik1 <- extract(estimated_rrdm, "contributionsI", permuted = F, inc_warmup = F,include = T)
r_eff1 <- relative_eff(exp(loglik1)) 
print(r_eff1)
loo1 <- loo(loglik1, r_eff = r_eff1)

print(loo1$pointwise)
print(loo1)

loglik2 <- extract(estimated_nrdm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff2 <- relative_eff(exp(loglik2)) 
loo2 <- loo(loglik2, r_eff = r_eff2)


loglik3 <- extract(estimated_rsdm, "contributionsI", permuted = F, inc_warmup = F,include = T)
r_eff3 <- relative_eff(exp(loglik3)) 
loo3 <- loo(loglik3, r_eff = r_eff3)



lpd_point <- cbind(
  loo1$pointwise[,'elpd_loo'],
  loo2$pointwise[,'elpd_loo'],
  loo3$pointwise[,'elpd_loo']
)

stacking_wts <- stacking_weights(lpd_point)
stacking_wts






# Additional checks

# correctness of the code 
rrdm_code <- get_stancode(estimated_rrdm)
cat(rrdm_code)

nrdm_code <- get_stancode(estimated_nrdm)
cat(nrdm_code)

rsdm_code <- get_stancode(estimated_rsdm)
cat(rsdm_code)


#elapsed time 
print(get_elapsed_time(estimated_rrdm)) 
print(get_elapsed_time(estimated_nrdm)) 
print(get_elapsed_time(estimated_rsdm))  

#tree depth
rrdm_params <- get_sampler_params(estimated_rrdm, inc_warmup = F)
max_treedepth_by_chain <- sapply(rrdm_params, function(x) max(x[, "treedepth__"]))
print(max_treedepth_by_chain)

nrdm_params <- get_sampler_params(estimated_nrdm, inc_warmup = F)
max_treedepth_by_chain <- sapply(nrdm_params, function(x) max(x[, "treedepth__"]))
print(max_treedepth_by_chain)

rsdm_params <- get_sampler_params(estimated_rsdm, inc_warmup = F)
max_treedepth_by_chain <- sapply(rsdm_params, function(x) max(x[, "treedepth__"]))
print(max_treedepth_by_chain)









