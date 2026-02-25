library(GDINA)

############################################################################################################

# Study 2 - misspesified structural component 
# 20 items 
Q_true_20 <- matrix(c(1,0,0,0,0,0,0, 0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0, 
                   0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1, 1,0,0,0,1,0,0,
                   0,1,0,0,1,0,0, 0,0,1,0,0,0,1, 0,0,0,1,0,1,0, 1,1,0,0,1,0,0,
                   1,0,1,0,0,0,1, 1,0,0,1,0,0,1, 0,1,1,0,0,0,1, 0,1,0,1,0,1,1,
                   0,0,1,1,0,1,1, 1,0,1,0,1,1,0, 1,1,0,1,1,1,0, 0,1,1,1,1,1,0),
                 ncol = 7, byrow = TRUE)

gs_high <- data.frame(guess=rep(0.1, J), slip=rep(0.1, J))
gs_low  <- data.frame(guess=rep(0.25, J), slip=rep(0.25, J))


# Define Hierarchical Structures (Mapping for 4 primary attributes)
# 1 indicates a prerequisite relationship (Row is prerequisite for Column)

# Linear: A -> B -> C -> D
linear_h <- matrix(c(0,1,0,0,  0,0,1,0,  0,0,0,1,  0,0,0,0), 4, 4, byrow=TRUE)

# Convergent: (A and B) -> C; C -> D
conv_h <- matrix(c(0,0,1,0,  0,0,1,0,  0,0,0,1,  0,0,0,0), 4, 4, byrow=TRUE)

# Divergent: A -> (B and C); (B and C) -> D
div_h <- matrix(c(0,1,1,0,  0,0,0,1,  0,0,0,1,  0,0,0,0), 4, 4, byrow=TRUE)

hierarchies <- list(Linear = linear_h, Convergent = conv_h, Divergent = div_h)


# Updated Design Factors for Study 2
design_factors_s2 <- expand.grid(
  Structure = c("Linear", "Convergent", "Divergent"),
  N = c(500, 1000),
  J = c(20, 40),
  qual_name = c("High", "Low"),
  a_type = c("Skill", "Misconception"),
  e_type = c("Omission", "Inclusion"),
  e_rate = c(0.05, 0.10, 0.20),
  stringsAsFactors = FALSE
)

n_reps <- 50
gs_high <- data.frame(guess=rep(0.1, J), slip=rep(0.1, J))
gs_low  <- data.frame(guess=rep(0.25, J), slip=rep(0.25, J))

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
                    att.struct = hierarchies[[struc_name]], # Apply Study 2 factor
                    no.bugs = 3)
    
    return(list(dat = extract(sim, "dat"), Q_used = Q_mis))
  })
  
  return(list(replications = condition_reps, condition = row))
})

# Save the results
save(results, file = "Study2_Full_Replications.RData")