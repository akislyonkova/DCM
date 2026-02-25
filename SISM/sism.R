library(GDINA)

############################################################################################################

# Study 1 - misspesified measurement component 

# 20 items 

Q_true <- matrix(c(1,0,0,0,0,0,0, 0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0, 
                   0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1, 1,0,0,0,1,0,0,
                   0,1,0,0,1,0,0, 0,0,1,0,0,0,1, 0,0,0,1,0,1,0, 1,1,0,0,1,0,0,
                   1,0,1,0,0,0,1, 1,0,0,1,0,0,1, 0,1,1,0,0,0,1, 0,1,0,1,0,1,1,
                   0,0,1,1,0,1,1, 1,0,1,0,1,1,0, 1,1,0,1,1,1,0, 0,1,1,1,1,1,0),
                 ncol = 7, byrow = TRUE)

J <- nrow(Q_true)
gs_high <- data.frame(guess=rep(0.1, J), slip=rep(0.1, J))
gs_low  <- data.frame(guess=rep(0.25, J), slip=rep(0.25, J))

# Design Factors 
design_factors <- expand.grid(
  N = c(500, 1000),
  qual_name = c("High", "Low"),
  a_type = c("Skill", "Misconception"),
  e_type = c("Omission", "Inclusion"),
  e_rate = c(0.05, 0.10, 0.20),
  stringsAsFactors = FALSE
)

item_qualities <- list(High = gs_high, Low = gs_low)

# Function to create misspecified Q-matrix 
create_misspecified_Q <- function(Q_true, a_type, e_type, e_rate) {
  Q_mis <- Q_true
  target_cols <- if(a_type == "Skill") 1:4 else 5:7
  
  # Get cells available for flipping based on error type
  target_value <- if(e_type == "Omission") 1 else 0         # debug later
  target_cells <- which(Q_mis[, target_cols] == target_value, arr.ind = TRUE)
  
  if (e_type == "Omission") {
    # Calculate row sums for the WHOLE Q-matrix
    row_sums <- rowSums(Q_mis)
    
    # Only keep cells where the row sum is > 1
    # This ensures Kj remains at least 1 after flipping a 1 to a 0
    valid_rows <- which(row_sums > 1)
    target_cells <- target_cells[target_cells[, "row"] %in% valid_rows, , drop = FALSE]
  }
  
  # Adjust column indices
  target_cells[, 2] <- target_cols[target_cells[, 2]]
    
  # Randomly select cells to flip
  n_flip <- ceiling(e_rate * nrow(target_cells))
  flip_idx <- sample(1:nrow(target_cells), n_flip)
  
  for (i in flip_idx) {
    row <- target_cells[i, 1]
    col <- target_cells[i, 2]
    Q_mis[row, col] <- ifelse(e_type == "Omission", 0, 1) # debug
  }
  
  return(Q_mis)
}

# Define number of replications
n_reps <- 50

# Simulating data
results <- apply(design_factors, 1, function(row) {
  N         <- as.numeric(row["N"])
  qual_name <- row["qual_name"]
  a_type    <- row["a_type"]
  e_type    <- row["e_type"]
  e_rate    <- as.numeric(row["e_rate"])
  
  # Log progress for the condition
  cat(sprintf("\nCondition: N=%d, Qual=%s, Attr=%s, Err=%s, Rate=%.2f\n", 
              N, qual_name, a_type, e_type, e_rate))
  
  condition_reps <- lapply(1:n_reps, function(rep) {
    
    # Print every 10th replication to show progress 
    if(rep %% 10 == 0) cat(paste0("..", rep))
    
    # Create a fresh misspecified Q-matrix for each replication 
    Q_mis <- create_misspecified_Q(Q_true, a_type, e_type, e_rate)
    
    # Generate Data based on the SISM model
    sim <- simGDINA(N = N, 
                    Q = Q_mis, 
                    gs.parm = item_qualities[[qual_name]],
                    model = "SISM", 
                    no.bugs = 3)
    
    dat <- extract(sim, "dat")
    
    # Return data and the specific Q used for this rep
    return(list(dat = dat, Q_used = Q_mis))
  })
  
  return(list(
    replications = condition_reps,
    condition = row
  ))
})

# Save the results
save(results, file = "Study1_Full_Replications.RData")


###########################################
# Debugging 





###########################################
# 40 items 






















