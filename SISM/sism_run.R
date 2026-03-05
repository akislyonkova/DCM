########################################################################################################

library(GDINA)
library(foreach)
library(doParallel)


fit <- GDINA(dat = your_data_here, 
             Q = q_matrix, 
             model = "SISM", 
             no.bugs = 3) 


n_cores <- parallel::detectCores() - 1 
cl <- makeCluster(n_cores)
registerDoParallel(cl)

load("Study1_Full.Rdata")

# Simulation Parameters
n_conditions <- 96
n_reps <- 50


final_results <- foreach(cond = 1:n_conditions, 
                         .packages = "GDINA",
                         .export = c("dat", "Q_used")) %dopar% {
                           
                           condition_reps <- vector("list", n_reps)
                           
                           for (rep in 1:n_reps) {
                             current_data <- dat[[cond]][[rep]]
                             
                             fit_attempt <- tryCatch({
                               GDINA(dat = current_data, 
                                     Q = Q_used, 
                                     model = "SISM", 
                                     no.bugs = 3, 
                                     verbose = 0)
                             }, error = function(e) return(NULL))
                             
                             if (!is.null(fit_attempt)) {
                               condition_reps[[rep]] <- list(
                                 estimates = coef(fit_attempt),
                                 fit_stats = mod_indices(fit_attempt),
                                 success = TRUE
                               )
                             } else {
                               condition_reps[[rep]] <- list(success = FALSE)
                             }
                           }
                           
                           return(condition_reps) # 
                         }


stopCluster(cl)

save(final_results, file = "parallel_sism_results.Rdata")
message("Simulation complete. Results saved.")