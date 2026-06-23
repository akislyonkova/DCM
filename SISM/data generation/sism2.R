############################################################################################################

# Study 2 - misspecified structural component 


# Part 1 - over-specifying the structure 

library(GDINA)

n_reps <- 100


design_factors <- expand.grid(
  N          = c(500, 1000),
  J          = c(20, 40),
  qual_name  = c("High", "Low"),
  hier_name  = names(hierarchies),
  stringsAsFactors = FALSE
)


sim2_data1 <- lapply(seq_len(nrow(design_factors)), function(i) {
  row       <- design_factors[i, ]
  N         <- row$N
  J         <- row$J
  qual_name <- row$qual_name
  hier_name <- row$hier_name
  
  Q_base <- if (J == 20) {
    all_Q_matrices[[hier_name]]$items_20
  } else {
    all_Q_matrices[[hier_name]]$items_40
  }
  
  if (qual_name == "High") {
    gs_parms <- data.frame(guess = rep(0.1,  J), slip = rep(0.1,  J))
  } else {
    gs_parms <- data.frame(guess = rep(0.25, J), slip = rep(0.25, J))
  }
  
  cat(sprintf("\nCondition: N=%d, J=%d, Qual=%s, Hierarchy=%s",
              N, J, qual_name, hier_name))
  
  condition_reps <- lapply(1:n_reps, function(rep) {
    
    set.seed(2026 + i * 1000 + rep)
    
    # Print every 10th replication to show progress
    if (rep %% 10 == 0) cat(paste0("..", rep))
    
    sim <- simGDINA(N       = N,
                    Q       = Q_base,
                    gs.parm = gs_parms,
                    model   = "SISM",
                    no.bugs = 3)
    
    dat      <- extract(sim, "dat")
    itemprob <- extract(sim, what = "catprob.parm")
    delta    <- extract(sim, what = "delta.parm")
    profiles <- extract(sim, what = "attribute")
    
    return(list(
      dat      = dat,
      itemprob = itemprob,
      delta    = delta,
      profiles = profiles
    ))
  })
  
  return(list(
    replications = condition_reps,
    condition    = row,
    Q_used       = Q_base
  ))
})


save(sim2_data1, file = "Study2_data1.RData")


# Part 2 - under-specifying the structure 
