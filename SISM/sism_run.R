########################################################################################################

library(GDINA)
library(foreach)
library(doParallel)


n_cores <- 4               ### check that the cluster accepts this command 
cl <- makeCluster(n_cores)
registerDoParallel(cl)

load("Study1_Full.Rdata")

# Simulation Parameters
n_conditions <- 96
n_reps <- 50


final_results <- foreach(cond = 1:n_conditions, 
                         .packages = "GDINA",
                         .export = c("dat", "Q_used")) %dopar% {  ### check what is extracted 
                           
                           condition_reps <- vector("list", n_reps)
                           current_Q <- Q_list[[cond]]            ### Q-list no such object 
                           
                           for (rep in 1:n_reps) {
                             current_data <- dat[[cond]][[rep]]
                             
                             fit_attempt <- tryCatch({
                               GDINA(dat = current_data, 
                                     Q = current_Q, 
                                     model = "SISM", 
                                     no.bugs = 3, 
                                     verbose = 0)
                             }, error = function(e) return(NULL))
                             
                             if (!is.null(fit_attempt)) {
                               condition_reps[[rep]] <- list(
                                 estimates = coef(fit_attempt),        ### check how to call the indices 
                                 fit_stats = mod_indices(fit_attempt), ### protect from crushing if 1 dataset fails 
                                 success = TRUE
                               )
                             } else {
                               condition_reps[[rep]] <- list(success = FALSE)
                             }
                           }
                           saveRDS(condition_reps, file = paste0("cond_", cond, ".rds"))
                           return(condition_reps) 
                         }


stopCluster(cl)

save(final_results, file = "parallel_sism_results.Rdata")
message("Simulation complete. Results saved.")