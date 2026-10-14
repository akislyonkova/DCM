
library(GDINA)
library(foreach)
library(doParallel)
library(doRNG)

getwd()

setwd("~/DCM/sism/")
output_dir <- path.expand("~/DCM/sism/")

n_cores <- parallel::detectCores() - 1

cl <- makeCluster(n_cores)
registerDoParallel(cl)

load("/home/akislyonkova/DCM/sism/Study2_Data2.RData")

n_conditions2 <- nrow(estimation_grid2)
n_reps        <- 100

# sanity checks on the loaded objects
stopifnot(length(sim2_true_data2) == n_conditions2,
          all(c("item_length") %in% names(estimation_grid2)))
print(estimation_grid2)

set.seed(2026)

final_results2_pt2 <- foreach(cond = 1:n_conditions2,
                              .packages = "GDINA") %dorng% {
                                
                                condition_reps <- vector("list", n_reps)
                                
                                current_length   <- estimation_grid2$item_length[cond]
                                current_data_idx <- cond   # rows of estimation_grid2 follow the same order as sim2_true_data2
                                
                                # Q-matrix used to generate the data (same Q at fit stage)
                                current_Q <- if (current_length == "items_20") convergent_Q20 else convergent_Q40
                                
                                for (rep in 1:n_reps) {
                                  current_data <- sim2_true_data2[[current_data_idx]]$replications[[rep]]$dat
                                  
                                  # UNSTRUCTURED fit: no att.str supplied, so all 2^K profiles are
                                  # permissible and the attributes are treated as unrestricted
                                  fit_attempt <- tryCatch({
                                    GDINA(dat = current_data,
                                          Q = current_Q,
                                          model = "SISM",
                                          no.bugs = 3,
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
                                saveRDS(condition_reps,
                                        file = file.path(output_dir, paste0("cond2_pt2_", cond, ".rds")))
                                message(sprintf("Part 2 condition %d complete", cond))
                                condition_reps
                              }
stopCluster(cl)
save(final_results2_pt2, file = "study2_results2.Rdata")
message("Part 2 simulation complete. Results saved.")
