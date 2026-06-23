########################################################################################################

library(GDINA)
library(foreach)
library(doParallel)
library(doRNG)

getwd()

setwd("~/DCM/sism/")

n_cores <- parallel::detectCores() - 1                      

cl <- makeCluster(n_cores)
registerDoParallel(cl)
load("/home/akislyonkova/DCM/sism/Study2_Data1.RData")

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
  
  return(Q_matrix)
}

exclusive_pairs <- list(
  "Type1"        = list(c(5, 1)),
  "Type2"        = list(c(1, 5), c(5, 2)),
  "Type3"        = list(c(1, 5), c(1, 6), c(5, 2), c(6, 2)),
  "Type4"        = list(c(1, 5), c(5, 2), c(5, 3))
)


all_Q_matrices <- lapply(exclusive_pairs, function(h) {
  items_20 <- generate_q(h, n_items = 20)
  items_40 <- rbind(items_20, items_20)
  rownames(items_40) <- paste0("Item_", 1:40)
  
  list(
    items_20 = items_20,
    items_40 = items_40
  )
})

estimation_factors <- expand.grid(
  true_data_idx = 1:8,
  hierarchy     = names(exclusive_pairs),
  stringsAsFactors = FALSE
)

n_conditions <- nrow(estimation_factors)
n_reps       <- 100

set.seed(2026)

final_results2_pt1 <- foreach(cond = 1:n_conditions, 
                              .packages = "GDINA",
                              .export = c("sim2_true_data1", "estimation_factors", "all_Q_matrices", "n_reps")) %dorng% {
                                
                                condition_reps <- vector("list", n_reps);
                                
                                for (rep in 1:n_reps) {
                                  q_type <- data[[cond]]$type       
                                  q_items <- data[[cond]]$items
                                  current_Q <- all_Q_matrices[[q_type]][[q_items]]
                                  #current_Q <- data[[cond]]$replications[[rep]]$Q_used
                                  current_data <- data[[cond]]$replications[[rep]]$dat
                                  
                                  fit_attempt <- tryCatch({
                                    GDINA(dat = current_data, 
                                          Q = current_Q, 
                                          model = "SISM", 
                                          no.bugs = 3,
                                          att.str = valid_profiles,       
                                          att.dist = "saturated",         
                                          verbose = 0)
                                  }, error = function(e) {
                                    warning(sprintf("GDINA fit failed — cond %d, rep %d: %s", cond, rep, e$message))
                                    return(NULL)
                                  })
                                  
                                  if (!is.null(fit_attempt)) {
                                    
                                    estimates  <- tryCatch(coef(fit_attempt),
                                                           error = function(e) { warning(sprintf("coef() failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                    
                                    fit_stats  <- tryCatch(modelfit(fit_attempt),
                                                           error = function(e) { warning(sprintf("modelfit() failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                    
                                    person_mp  <- tryCatch(personparm(fit_attempt, what = "mp"),
                                                           error = function(e) { warning(sprintf("personparm(mp) failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                    
                                    class_prev <- tryCatch(coef(fit_attempt, what = "lambda"),
                                                           error = function(e) { warning(sprintf("coef(lambda) failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                    
                                    profiles   <- tryCatch(personparm(fit_attempt, what = "EAP"),
                                                           error = function(e) { warning(sprintf("personparm(EAP) failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                    
                                    condition_reps[[rep]] <- list(
                                      estimates  = estimates,
                                      fit_stats  = fit_stats,
                                      person_mp  = person_mp,
                                      class_prev = class_prev,
                                      profiles   = profiles,
                                      success    = TRUE
                                    )
                                    
                                  } else {
                                    condition_reps[[rep]] <- list(success = FALSE)
                                  }
                                }
                                saveRDS(condition_reps, file = paste0("cond_", cond, ".rds")) 
                                message(sprintf("Condition %d complete", cond))
                                condition_reps 
                              }
stopCluster(cl)
save(final_results2_pt1, file = "study2_results1.Rdata")
message("Simulation complete. Results saved.")

#############################################################################################