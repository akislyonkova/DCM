############################################################################################################

# Study 2 - misspecified structural component 

library(GDINA)

n_reps <- 100

generate_q_matrix <- function(hierarchy_list, K = 7, n_items = 20) {
  
  patterns <- att.structure(hierarchy.list = hierarchy_list, K = K)
  
  Q_indices <- sample(1:nrow(patterns$att.str), n_items, replace = TRUE)
  Q_matrix <- patterns$att.str[Q_indices, ]
  
  colnames(Q_matrix) <- c("S1", "S2", "S3", "S4", "M1", "M2", "M3")
  rownames(Q_matrix) <- paste0("Item_", 1:n_items)
  
  return(Q_matrix)
}


set.seed(456)


hierarchies <- list(
  "Linear_A_to_B"        = list(c(1, 2)),
  "Linear_B_to_A"        = list(c(2, 1)),
  "Convergent_Basic"     = list(c(1, 3), c(2, 3)),
  "Convergent_Complex"   = list(c(1, 2), c(2, 4), c(3, 4)),
  "Divergent_Basic"      = list(c(1, 2), c(1, 3)),
  "Divergent_Complex"    = list(c(1, 2), c(1, 3), c(2, 4)) 
)


all_Q_matrices <- lapply(hierarchies, function(h) {
  items_20 <- generate_q_matrix(h, n_items = 20)
  items_40 <- rbind(items_20, items_20)
  rownames(items_40) <- paste0("Item_", 1:40)
  
  list(
    items_20 = items_20,
    items_40 = items_40
  )
})


design_factors <- expand.grid(
  N          = c(500, 1000),
  J          = c(20, 40),
  qual_name  = c("High", "Low"),
  hier_name  = names(hierarchies),
  stringsAsFactors = FALSE
)


sim2_data <- lapply(seq_len(nrow(design_factors)), function(i) {
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
    
    set.seed(2026 + rep)
    
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
      Q_used   = Q_base,   
      itemprob = itemprob,
      delta    = delta,
      profiles = profiles
    ))
  })
  
  return(list(
    replications = condition_reps,
    condition    = row
  ))
})


save(sim2_data, file = "Study2_data.RData")


