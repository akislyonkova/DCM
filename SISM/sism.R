library(GDINA)

###############################################################
# The Q-matrix used in Kuo, et al (2018)
# The first four columns are for Attributes 1-4
# The last three columns are for Bugs 1-3
Q <- matrix(c(1,0,0,0,0,0,0,
              0,1,0,0,0,0,0,
              0,0,1,0,0,0,0,
              0,0,0,1,0,0,0,
              0,0,0,0,1,0,0,
              0,0,0,0,0,1,0,
              0,0,0,0,0,0,1,
              1,0,0,0,1,0,0,
              0,1,0,0,1,0,0,
              0,0,1,0,0,0,1,
              0,0,0,1,0,1,0,
              1,1,0,0,1,0,0,
              1,0,1,0,0,0,1,
              1,0,0,1,0,0,1,
              0,1,1,0,0,0,1,
              0,1,0,1,0,1,1,
              0,0,1,1,0,1,1,
              1,0,1,0,1,1,0,
              1,1,0,1,1,1,0,
              0,1,1,1,1,1,0),ncol = 7,byrow = TRUE)
J <- nrow(Q)
N <- 500
gs_high <- data.frame(guess=rep(0.1,J),slip=rep(0.1,J))  # high quality 
gs_low <- data.frame(guess=rep(0.25,J),slip=rep(0.25,J)) # low quality
sim <- simGDINA(N,Q,gs.parm = gs_low,model = "SISM",no.bugs=3)
# True item success probabilities
extract(sim,what = "catprob.parm")
# True delta parameters
extract(sim,what = "delta.parm")
# simulated data
extract(sim,what = "dat")
# simulated attributes
attribute <- extract(sim,what = "attribute")



# Estimate SISM 
start.time <- Sys.time()
fit_sism <- GDINA(dat = sim$dat, 
                  Q = Q, 
                  model = "SISM", 
                  no.bugs = 3) # Must match Q colname
end.time <- Sys.time()
time.taken <- end.time - start.time

summary(fit_sism)



############################################################################################################

# Study 1 - misspesified measurement component 

Q_true <- matrix(c(1,0,0,0,0,0,0, 0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0, 
                   0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1, 1,0,0,0,1,0,0,
                   0,1,0,0,1,0,0, 0,0,1,0,0,0,1, 0,0,0,1,0,1,0, 1,1,0,0,1,0,0,
                   1,0,1,0,0,0,1, 1,0,0,1,0,0,1, 0,1,1,0,0,0,1, 0,1,0,1,0,1,1,
                   0,0,1,1,0,1,1, 1,0,1,0,1,1,0, 1,1,0,1,1,1,0, 0,1,1,1,1,1,0),
                 ncol = 7, byrow = TRUE)

J <- nrow(Q_true)
gs_high <- data.frame(guess=rep(0.1, J), slip=rep(0.1, J))
gs_low  <- data.frame(guess=rep(0.25, J), slip=rep(0.25, J))

# --- 2. Design Factors from Research Design ---
design_factors <- expand.grid(
  N = c(500, 1000),
  qual_name = c("High", "Low"),
  a_type = c("Skill", "Misconception"),
  e_type = c("Omission", "Inclusion"),
  e_rate = c(0.05, 0.10, 0.20),
  stringsAsFactors = FALSE
)

item_qualities <- list(High = gs_high, Low = gs_low)

# --- 3. Function to create misspecified Q-matrix ---
create_misspecified_Q <- function(Q_true, a_type, e_type, e_rate) {
  Q_mis <- Q_true
  target_cols <- if(a_type == "Skill") 1:4 else 5:7
  
  # Get cells available for flipping based on error type
  target_value <- if(e_type == "Omission") 1 else 0
  target_cells <- which(Q_mis[, target_cols] == target_value, arr.ind = TRUE)
  
  if (nrow(target_cells) > 0) {
    # Adjust column indices
    target_cells[, 2] <- target_cols[target_cells[, 2]]
    
    # Randomly select cells to flip
    n_flip <- round(e_rate * nrow(target_cells))
    flip_idx <- sample(1:nrow(target_cells), n_flip)
    
    for (i in flip_idx) {
      row <- target_cells[i, 1]
      col <- target_cells[i, 2]
      Q_mis[row, col] <- ifelse(e_type == "Omission", 0, 1)
    }
  }
  
  return(Q_mis)
}

# --- 4. Simulation using apply approach ---
results <- apply(design_factors, 1, function(row) {
  N <- as.numeric(row["N"])
  qual_name <- row["qual_name"]
  a_type <- row["a_type"]
  e_type <- row["e_type"]
  e_rate <- as.numeric(row["e_rate"])
  
  # Generate True Data
  sim <- simGDINA(N, Q_true, gs.parm = item_qualities[[qual_name]], 
                  model = "SISM", no.bugs = 3)
  dat <- extract(sim, "dat")
  
  # Create Misspecified Q-matrix
  Q_mis <- create_misspecified_Q(Q_true, a_type, e_type, e_rate)
  
  # Estimate Model
  fit <- GDINA(dat = dat, Q = Q_mis, model = "SISM", no.bugs = 3)
  
  # Return results with metadata
  list(
    params = row,
    fit = fit,
    # Add any other output you want to store
    condition_id = paste(N, qual_name, a_type, e_type, e_rate, sep = "_")
  )
})

# Name the results list for easier access
names(results) <- sapply(results, function(x) x$condition_id)
