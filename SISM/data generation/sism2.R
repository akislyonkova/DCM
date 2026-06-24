############################################################################################################

# Study 2 - misspecified structural component 


# Part 1 - over-specifying the structure 

library(GDINA)

set.seed(2026)


generate_unstr_q <- function(K = 7, n_items = 20) {
  
  patterns <- att.structure(K = K)
  valid_patterns <- patterns$att.str[rowSums(patterns$att.str) > 0, ]
  
  I_matrix <- diag(K)
  
  n_remaining <- n_items - K
  
  valid_Q <- FALSE
  while (!valid_Q) {
    Q_indices <- sample(1:nrow(valid_patterns), n_remaining, replace = TRUE)
    Q_sampled <- valid_patterns[Q_indices, ]
    
    Q_matrix <- rbind(I_matrix, Q_sampled)
    
    if (all(colSums(Q_matrix) > 0)) {
      valid_Q <- TRUE
    }
  }
  
  colnames(Q_matrix) <- c("S1", "S2", "S3", "S4", "M1", "M2", "M3")
  rownames(Q_matrix) <- paste0("Item_", 1:n_items)
  
  return(Q_matrix)
}

unstructured_Q20 <- generate_unstr_q(K = 7, n_items = 20)
unstructured_Q40 <- rbind(unstructured_Q20, unstructured_Q20)
rownames(unstructured_Q40) <- paste0("Item_", 1:40)

generate_q <- function(exclusive_pairs, K = 7, n_items = 20) {
  
  patterns <- att.structure(K = K)
  valid_patterns <- patterns$att.str[rowSums(patterns$att.str) > 0, ]
  
  for (pair in exclusive_pairs) {
    attr1 <- pair[1]
    attr2 <- pair[2]
    
    valid_patterns <- valid_patterns[!(valid_patterns[, attr1] == 1 & valid_patterns[, attr2] == 1), ]
  }
  
  I_matrix <- diag(K)
  n_remaining <- n_items - K
  
  valid_Q <- FALSE
  while (!valid_Q) {
    Q_indices <- sample(1:nrow(valid_patterns), n_remaining, replace = TRUE)
    Q_sampled <- valid_patterns[Q_indices, ]
    
    Q_matrix <- rbind(I_matrix, Q_sampled)
    
    if (all(colSums(Q_matrix) > 0)) {
      valid_Q <- TRUE
    }
  }
  
  colnames(Q_matrix) <- c("S1", "S2", "S3", "S4", "M1", "M2", "M3")
  rownames(Q_matrix) <- paste0("Item_", 1:n_items)
  
  return(list(Q = Q_matrix, profiles = valid_patterns))
}

exclusive_pairs <- list(
  "Type1"        = list(c(5, 1)),
  "Type2"        = list(c(1, 5), c(5, 2)),
  "Type3"        = list(c(1, 5), c(1, 6), c(5, 2), c(6, 2)),
  "Type4"        = list(c(1, 5), c(5, 2), c(5, 3))
)


all_Q_matrices <- lapply(exclusive_pairs, function(h) {
  result_20 <- generate_q(h, n_items = 20)
  items_20_Q <- result_20$Q
  items_40_Q <- rbind(items_20_Q, items_20_Q)
  rownames(items_40_Q) <- paste0("Item_", 1:40)
  
  list(
    items_20 = items_20_Q,
    items_40 = items_40_Q,
    valid_profiles = result_20$profiles
  )
})

estimation_factors <- expand.grid(
  true_data_idx    = 1:8,
  hierarchy        = names(exclusive_pairs),
  stringsAsFactors = FALSE
)


n_reps <- 100

design_factors <- expand.grid(
  N          = c(500, 1000),
  J          = c(20, 40),
  qual_name  = c("High", "Low"),
  stringsAsFactors = FALSE
)

design_factors$true_data_idx <- 1:nrow(design_factors)

sim2_true_data1 <- lapply(seq_len(nrow(design_factors)), function(i) {
  row       <- design_factors[i, ]
  N         <- row$N
  J         <- row$J
  qual_name <- row$qual_name
  
  Q_base <- if (J == 20) {
    unstructured_Q20
  } else {
    unstructured_Q40
  }
  
  if (qual_name == "High") {
    gs_parms <- data.frame(guess = rep(0.1,  J), slip = rep(0.1,  J))
  } else {
    gs_parms <- data.frame(guess = rep(0.25, J), slip = rep(0.25, J))
  }
  
  cat(sprintf("\nGenerating Data for Condition %d/8: N=%d, J=%d, Qual=%s",
              i, N, J, qual_name))
  
  condition_reps <- lapply(1:n_reps, function(rep) {
    
    set.seed(2026 + i * 1000 + rep)
    
    if (rep %% 10 == 0) cat(paste0("..", rep))
    
    sim <- simGDINA(N        = N,
                    Q        = Q_base,
                    gs.parm  = gs_parms,
                    model    = "SISM",
                    no.bugs  = 3) 
    
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

save(sim2_true_data1, all_Q_matrices, design_factors, file = "Study2_Data1.RData")


# Part 2 - under-specifying the structure 
