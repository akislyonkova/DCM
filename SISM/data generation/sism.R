library(GDINA)

############################################################################################################

# Study 1 - misspecified measurement component 

set.seed(2026)

n_reps <- 50

Q_20 <- matrix(c(1,0,0,0,0,0,0, 0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0, 
                 0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1, 1,0,0,0,1,0,0,
                 0,1,0,0,1,0,0, 0,0,1,0,0,0,1, 0,0,0,1,0,1,0, 1,1,0,0,1,0,0,
                 1,0,1,0,0,0,1, 1,0,0,1,0,0,1, 0,1,1,0,0,0,1, 0,1,0,1,0,1,1,
                 0,0,1,1,0,1,1, 1,0,1,0,1,1,0, 1,1,0,1,1,1,0, 0,1,1,1,1,1,0),
               ncol = 7, byrow = TRUE)

Q_40 <- rbind(Q_20, Q_20)

# Design Factors (Added J for item length)
design_factors <- expand.grid(
  N = c(500, 1000),
  J = c(20, 40),
  qual_name = c("High", "Low"),
  a_type = c("Skill", "Misconception"),
  e_type = c("Omission", "Inclusion"),
  e_rate = c(0.05, 0.10, 0.20),
  stringsAsFactors = FALSE
)

# Function to create misspecified Q-matrix 
create_misspecified_Q <- function(Q_base, a_type, e_type, e_rate) {
  Q_mis <- Q_base
  target_cols <- if(a_type == "Skill") 1:4 else 5:7
  
  target_value <- if(e_type == "Omission") 1 else 0         # safety checks later
  target_cells <- which(Q_mis[, target_cols] == target_value, arr.ind = TRUE)
  
  if (e_type == "Omission") {
    row_sums <- rowSums(Q_mis)
    
    valid_rows <- which(row_sums > 1)
    target_cells <- target_cells[target_cells[, "row"] %in% valid_rows, , drop = FALSE]
  }
  
  target_cells[, 2] <- target_cols[target_cells[, 2]]
  
  n_flip <- ceiling(e_rate * nrow(target_cells))
  flip_idx <- sample(1:nrow(target_cells), n_flip)
  
  for (i in flip_idx) {
    row <- target_cells[i, 1]
    col <- target_cells[i, 2]
    Q_mis[row, col] <- ifelse(e_type == "Omission", 0, 1) # safety checks later
  }
  
  return(Q_mis)
}

# Simulating data
results <- apply(design_factors, 1, function(row) {
  N         <- as.numeric(row["N"])
  J_val     <- as.numeric(row["J"])
  qual_name <- row["qual_name"]
  a_type    <- row["a_type"]
  e_type    <- row["e_type"]
  e_rate    <- as.numeric(row["e_rate"])
  
  # Select the correct base Q-matrix for this condition
  Q_base <- if(J_val == 20) Q_20 else Q_40
  
  # Generate the item parameters matching the number of items (J_val)
  if(qual_name == "High") {
    gs_parms <- data.frame(guess=rep(0.1, J_val), slip=rep(0.1, J_val))
  } else {
    gs_parms <- data.frame(guess=rep(0.25, J_val), slip=rep(0.25, J_val))
  }
  
  # Log progress for the condition
  cat(sprintf("\nCondition: N=%d, J=%d, Qual=%s, Attr=%s, Err=%s, Rate=%.2f\n", 
              N, J_val, qual_name, a_type, e_type, e_rate))
  
  condition_reps <- lapply(1:n_reps, function(rep) {
    
    # Print every 10th replication to show progress 
    if(rep %% 10 == 0) cat(paste0("..", rep))
    
    # Create a fresh misspecified Q-matrix for each replication based on Q_base
    Q_mis <- create_misspecified_Q(Q_base, a_type, e_type, e_rate)
    
    # Generate Data based on the SISM model
    sim <- simGDINA(N = N, 
                    Q = Q_mis, 
                    gs.parm = gs_parms,
                    model = "SISM", 
                    no.bugs = 3)
    
    dat <- extract(sim, "dat")
    itemprob <- extract(sim, what = "catprob.parm")
    delta <- extract(sim, what = "delta.parm")
    profiles <- extract(sim,what = "attribute")
    
    # Return data and the specific Q used for this rep
    return(list(dat = dat, Q_used = Q_mis, itemprob = itemprob, delta = delta, profiles = profiles))
  })
  
  return(list(
    replications = condition_reps,
    condition = row
  ))
})

# Save the results
save(results, file = "Study1_Full_Replications.RData")

