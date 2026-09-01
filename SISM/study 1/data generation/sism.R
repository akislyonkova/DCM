library(GDINA)

############################################################################################################

# Study 1 - misspecified measurement component 


n_reps <- 100

Q_20 <- matrix(c(1,0,0,0,0,0,0, 0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0, 
                 0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1, 1,0,0,0,1,0,0,
                 0,1,0,0,1,0,0, 0,0,1,0,0,0,1, 0,0,0,1,0,1,0, 1,1,0,0,1,0,0,
                 1,0,1,0,0,0,1, 1,0,0,1,0,0,1, 0,1,1,0,0,0,1, 0,1,0,1,0,1,1,
                 0,0,1,1,0,1,1, 1,0,1,0,1,1,0, 1,1,0,1,1,1,0, 0,1,1,1,1,1,0),
               ncol = 7, byrow = TRUE)

Q_40 <- rbind(Q_20, Q_20)


design_factors <- expand.grid(
  N = c(500, 1000),
  J = c(20, 40),
  qual_name = c("High", "Low"),
  a_type = c("Skill", "Misconception"),
  e_type = c("Omission", "Inclusion"),
  e_rate = c(0.05, 0.15),
  stringsAsFactors = FALSE
)

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


data <- apply(design_factors, 1, function(row) {
  N         <- as.numeric(row["N"])
  J_val     <- as.numeric(row["J"])
  qual_name <- row["qual_name"]
  a_type    <- row["a_type"]
  e_type    <- row["e_type"]
  e_rate    <- as.numeric(row["e_rate"])
  
  Q_base <- if(J_val == 20) Q_20 else Q_40
  
  if(qual_name == "High") {
    gs_parms <- data.frame(guess=rep(0.1, J_val), slip=rep(0.1, J_val))
  } else {
    gs_parms <- data.frame(guess=rep(0.25, J_val), slip=rep(0.25, J_val))
  }
  
  cat(sprintf("\nCondition: N=%d, J=%d, Qual=%s, Attr=%s, Err=%s, Rate=%.2f\n", 
              N, J_val, qual_name, a_type, e_type, e_rate))
  
  condition_reps <- lapply(1:n_reps, function(rep) {
    
    set.seed(2026 + rep)
    
    # Print every 10th replication to show progress 
    if(rep %% 10 == 0) cat(paste0("..", rep))
    
    Q_mis <- create_misspecified_Q(Q_base, a_type, e_type, e_rate)
    
    sim <- simGDINA(N = N, 
                    Q = Q_mis, 
                    gs.parm = gs_parms,
                    model = "SISM", 
                    no.bugs = 3)
    
    dat <- extract(sim, "dat")
    itemprob <- extract(sim, what = "catprob.parm")
    delta <- extract(sim, what = "delta.parm")
    profiles <- extract(sim,what = "attribute")
    
    return(list(dat = dat, Q_used = Q_mis, itemprob = itemprob, delta = delta, profiles = profiles))
  })
  
  return(list(
    replications = condition_reps,
    condition = row
  ))
})


save(data, file = "Study1_data.RData")

