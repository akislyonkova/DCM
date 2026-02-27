library(GDINA)

############################################################################################################

# Study 2 - misspesified structural component 
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
item_qualities <- list(High = gs_high, Low = gs_low)

# Define J_max based on your largest test length factor
J_max <- 40 

# Generate parameters for the maximum length
gs_high <- data.frame(guess = rep(0.1, J_max), slip = rep(0.1, J_max))
gs_low  <- data.frame(guess = rep(0.25, J_max), slip = rep(0.25, J_max))

item_qualities <- list(High = gs_high, Low = gs_low)

# Define Hierarchical Structures (Mapping for 4 primary attributes)
# 1 indicates a prerequisite relationship (Row is prerequisite for Column)

## Adjacency matrices for 4 attributes (K=4)
# Initialize 7x7 empty matrices
lin_mat <- con_mat <- div_mat <- matrix(0, 7, 7)

# Linear: A -> B -> C -> D (using first 4 columns)
lin_mat[1,2] <- lin_mat[2,3] <- lin_mat[3,4] <- 1

# Convergent: (A & B) -> C; C -> D
con_mat[1,3] <- con_mat[2,3] <- con_mat[3,4] <- 1

# Divergent: A -> (B & C); (B & C) -> D
div_mat[1,2] <- div_mat[1,3] <- div_mat[2,4] <- div_mat[3,4] <- 1

adj_matrices <- list(Linear = lin_mat, Convergent = con_mat, Divergent = div_mat)


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


# Updated Design Factors for Study 2
design_factors_s2 <- expand.grid(
  Structure = c("Linear", "Convergent", "Divergent"),
  N = c(500, 1000),
  J = c(20, 40),
  qual_name = c("High", "Low"),
  e_type = c("Omission", "Inclusion"),
  e_rate = c(0.05, 0.10, 0.20),
  stringsAsFactors = FALSE
)

n_reps <- 50

results_study2 <- apply(design_factors_s2, 1, function(row) {
  # Parse row values
  struc_name <- row["Structure"]
  N          <- as.numeric(row["N"])
  J_len      <- as.numeric(row["J"])
  qual_name  <- row["qual_name"]
  e_type     <- row["e_type"]
  e_rate     <- as.numeric(row["e_rate"])
  
  cat(sprintf("\nRunning: %s | J=%d | N=%d | %s Err (%.2f)", 
              struc_name, J_len, N, e_type, e_rate))
  
  # 1. Select appropriate base Q-matrix for Test Length (J)
  # (Ensure you have Q_true_20 and Q_true_40 defined in your environment)
  current_Q_true <- if(J_len == 20) Q_true else Q_true_40 
  
  # 2. Replications
  condition_reps <- lapply(1:n_reps, function(rep) {
    
    # Create Misspecified Q
    Q_mis <- create_misspecified_Q(current_Q_true, row["a_type"], e_type, e_rate)
    
    # Generate Data with Hierarchy
    # The 'att.struct' argument applies the Hierarchical Structure to the SISM
    sim <- simGDINA(N = N, 
                    Q = Q_mis, 
                    gs.parm = item_qualities[[qual_name]][1:J_len, ],
                    model = "SISM", 
                    att.struct = hierarchies[[struc_name]], 
                    no.bugs = 3)
    
    return(list(dat = extract(sim, "dat"), Q_used = Q_mis))
  })
  
  return(list(replications = condition_reps, condition = row))
})

# Save the results
save(results, file = "Study2_Full_Replications.RData")

#############################################################################################################
# Debugging 
Q_true_40 <- rbind(Q_true, Q_true)

create_misspecified_Q <- function(Q_true, e_type, e_rate) {
  Q_mis <- Q_true
  # Now targets all 7 columns in the Q-matrix
  target_cols <- 1:ncol(Q_true)
  
  # Identify target cells (1s for Omission, 0s for Inclusion)
  target_value <- if(e_type == "Omission") 1 else 0
  target_cells <- which(Q_mis[, target_cols] == target_value, arr.ind = TRUE)
  
  # Ensure target_cells uses the correct global column indices
  target_cells[, 2] <- target_cols[target_cells[, 2]]
  
  if (e_type == "Omission") {
    # Calculate row sums to identify items measuring only one attribute
    row_sums <- rowSums(Q_mis)
    
    # Filter: Only allow flipping a '1' if the item measures > 1 attribute
    # This prevents Kj = 0 errors in simGDINA
    valid_rows <- which(row_sums > 1)
    target_cells <- target_cells[target_cells[, "row"] %in% valid_rows, , drop = FALSE]
  }
  
  # Safety check for empty target sets
  if (nrow(target_cells) == 0) return(Q_mis)
  
  # Randomly select cells to flip
  n_flip <- ceiling(e_rate * nrow(target_cells))
  # Ensure we don't try to sample more than available
  n_flip <- min(n_flip, nrow(target_cells)) 
  
  flip_idx <- sample(1:nrow(target_cells), n_flip)
  
  for (i in flip_idx) {
    curr_row <- target_cells[i, 1]
    curr_col <- target_cells[i, 2]
    Q_mis[curr_row, curr_col] <- if(e_type == "Omission") 0 else 1
  }
  
  return(Q_mis)
}


results_study2 <- apply(design_factors_s2, 1, function(row) {
  # Parse design factors
  struc_name <- row["Structure"]
  N          <- as.numeric(row["N"])
  J_len      <- as.numeric(row["J"])
  qual_name  <- row["qual_name"]
  e_type     <- row["e_type"]
  e_rate     <- as.numeric(row["e_rate"])
  
  # 1. Generate the restricted attribute space
  # In GDINA, the valid mastery patterns are in $att.str
  struc_output <- att.structure(adj_matrices[[struc_name]], K = 7)
  valid_patterns <- struc_output$att.str
  
  cat(sprintf("\nStructure: %s | Valid Patterns: %d", 
              struc_name, nrow(valid_patterns)))
  
  condition_reps <- lapply(1:n_reps, function(rep) {
    # Match the Q-matrix to the test length factor
    current_Q_true <- if(J_len == 20) Q_true else Q_true_40 
    
    Q_mis <- create_misspecified_Q(current_Q_true,
                                   row["e_type"], as.numeric(row["e_rate"]))
    
    # 2. Sample N rows from the valid patterns
    sampled_attrs <- valid_patterns[sample(nrow(valid_patterns), N, replace = TRUE), ]
    
    # 3. Simulate SISM data
    sim <- simGDINA(N = N, 
                    Q = Q_mis, 
                    gs.parm = item_qualities[[qual_name]][1:J_len, ],
                    model = "SISM", 
                    attribute = sampled_attrs,
                    no.bugs = 3)
    
    return(list(dat = extract(sim, "dat"), Q_used = Q_mis))
  })
  
  return(list(replications = condition_reps, condition = row))
})

#########################################################################################
# Time estimate 
library(GDINA)

Q_true <- matrix(c(1,0,0,0,0,0,0, 0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0, 
                   0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1, 1,0,0,0,1,0,0,
                   0,1,0,0,1,0,0, 0,0,1,0,0,0,1, 0,0,0,1,0,1,0, 1,1,0,0,1,0,0,
                   1,0,1,0,0,0,1, 1,0,0,1,0,0,1, 0,1,1,0,0,0,1, 0,1,0,1,0,1,1,
                   0,0,1,1,0,1,1, 1,0,1,0,1,1,0, 1,1,0,1,1,1,0, 0,1,1,1,1,1,0),
                 ncol = 7, byrow = TRUE)


K <- 7
linear <- list(c(1,2),c(2,3),c(3,4),c(4,5),c(5,6),c(6,7))
linear_attr <- att.structure(linear, K)


N <- 500
all_patterns <- attributepattern(K) 


sampled_rows <- sample(1:nrow(all_patterns), size = N, replace = TRUE, prob = linear_attr$att.prob)
simulated_attributes <- all_patterns[sampled_rows, ]


J <- nrow(Q_true)
gs_high <- data.frame(guess = rep(0.1, J), slip = rep(0.1, J))

sim <- simGDINA(N = N, 
                Q = Q_true, 
                gs.parm = gs_high,
                model = "SISM", 
                attribute = simulated_attributes, # Pass the matrix here instead of att.str
                no.bugs = 3)
dat <- sim$dat

start.time <- Sys.time()

fit_sism <- GDINA(dat = dat, 
                  Q = Q_true, 
                  model = "SISM", 
                  no.bugs = 3) 
end.time <- Sys.time()
time.taken <- end.time - start.time

summary(fit_sism)