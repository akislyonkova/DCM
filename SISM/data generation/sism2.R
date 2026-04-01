library(GDINA)

############################################################################################################

# Study 2 - misspesified structural component 

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
  stringsAsFactors = FALSE
)


sim2_data <- lapply(seq_len(nrow(design_factors)), 1, function(row) {
  row       <- design_factors[i, ]
  N         <- row$N
  J         <- row$J
  qual_name <- row$qual_name
  
  Q_base <- if(J == 20) Q_20 else Q_40
  
  if(qual_name == "High") {
    gs_parms <- data.frame(guess=rep(0.1, J), slip=rep(0.1, J))
  } else {
    gs_parms <- data.frame(guess=rep(0.25, J), slip=rep(0.25, J))
  }
  
  cat(sprintf("\nCondition: N=%d, J=%d, Qual=%s", 
              N, J, qual_name))
  
  condition_reps <- lapply(1:n_reps, function(rep) {
    
    set.seed(2026 + rep)
    
    # Print every 10th replication to show progress 
    if(rep %% 10 == 0) cat(paste0("..", rep))
    
    sim <- simGDINA(N = N, 
                    Q = Q_base, 
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


save(sim2_data, file = "Study2_data.RData")












########################################################################################################

#### Linear A → B
skill_hierarchy <- list(c(1, 2), c(2, 3), c(3, 4))

patterns <- att.structure(hierarchy.list = skill_hierarchy, K = 7)

set.seed(456)
Q_indices <- sample(1:nrow(patterns$att.str), 20, replace = TRUE)
Q_matrix <- patterns$att.str[Q_indices, ]

colnames(Q_matrix) <- c("S1", "S2", "S3", "S4", "M1", "M2", "M3")
rownames(Q_matrix) <- paste0("Item_", 1:20)

print(Q_matrix)


#### Linear B → A

skill_hierarchy <- list(c(4, 3), c(3, 2), c(2, 1))

patterns <- att.structure(hierarchy.list = skill_hierarchy, K = 7)

set.seed(456)
Q_indices <- sample(1:nrow(patterns$att.str), 20, replace = TRUE)
Q_matrix <- patterns$att.str[Q_indices, ]

colnames(Q_matrix) <- c("S1", "S2", "S3", "S4", "M1", "M2", "M3")
rownames(Q_matrix) <- paste0("Item_", 1:20)

print(Q_matrix)


### Convergent (A + B) → C and then C → D:
converg_conjunctive <- list(
  c(1, 3), # A is a prerequisite for C
  c(2, 3), # B is a prerequisite for C
  c(3, 4)  # C is a prerequisite for D
)
patterns <- att.structure(hierarchy.list = converg, K = 7)

set.seed(456)
Q_indices <- sample(1:nrow(patterns$att.str), 20, replace = TRUE)
Q_matrix <- patterns$att.str[Q_indices, ]

colnames(Q_matrix) <- c("S1", "S2", "S3", "S4", "M1", "M2", "M3")
rownames(Q_matrix) <- paste0("Item_", 1:20)

print(Q_matrix)

### Convergent Complex/Layered: S1 -> S2; (S2 + S3) -> S4
complex_logic <- list(
  c(1, 2), 
  c(2, 4), 
  c(3, 4)
)
patterns <- att.structure(hierarchy.list = converg, K = 7)

set.seed(456)
Q_indices <- sample(1:nrow(patterns$att.str), 20, replace = TRUE)
Q_matrix <- patterns$att.str[Q_indices, ]

colnames(Q_matrix) <- c("S1", "S2", "S3", "S4", "M1", "M2", "M3")
rownames(Q_matrix) <- paste0("Item_", 1:20)

print(Q_matrix)

