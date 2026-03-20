########################################################################################################
library(GDINA)
library(foreach)
library(doParallel)
n_cores <- parallel::detectCores() - 1                      # for local PC
# n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))  # for cluster 
cl <- makeCluster(n_cores)
registerDoParallel(cl)
load("Study1_Full.Rdata")
# Simulation Parameters
n_conditions <- 96
n_reps <- 50
# n_conditions <- 2
# n_reps <- 1
final_results <- foreach(cond = 1:n_conditions, 
                         .packages = "GDINA",
                         .export = c("results")) %dopar% {   
                           
                           condition_reps <- vector("list", n_reps)
                           current_Q <- results[[1]]$replications[[1]]$Q_used                
                           
                           for (rep in 1:n_reps) {
                             current_data <- results[[cond]]$replications[[rep]]$dat
                             
                             fit_attempt <- tryCatch({
                               GDINA(dat = current_data, 
                                     Q = current_Q, 
                                     model = "SISM", 
                                     no.bugs = 3, 
                                     verbose = 0)
                             }, error = function(e) {
                               message(sprintf("GDINA fit failed — cond %d, rep %d: %s", cond, rep, e$message))
                               return(NULL)
                             })
                             
                             if (!is.null(fit_attempt)) {
                               
                               estimates  <- tryCatch(coef(fit_attempt),
                                                      error = function(e) { message(sprintf("coef() failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                               
                               fit_stats  <- tryCatch(modelfit(fit_attempt),
                                                      error = function(e) { message(sprintf("modelfit() failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                               
                               personparm <- tryCatch(personparm(fit_attempt, what = "mp"),
                                                      error = function(e) { message(sprintf("personparm(mp) failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                               
                               class_prev <- tryCatch(coef(fit_attempt, what = "lambda"),
                                                      error = function(e) { message(sprintf("coef(lambda) failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                               
                               profiles   <- tryCatch(personparm(fit_attempt, what = "EAP"),
                                                      error = function(e) { message(sprintf("personparm(EAP) failed — cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                               
                               condition_reps[[rep]] <- list(
                                 estimates  = estimates,
                                 fit_stats  = fit_stats,
                                 personparm = personparm,
                                 class_prev = class_prev,
                                 profiles   = profiles,
                                 success    = TRUE
                               )
                               
                             } else {
                               condition_reps[[rep]] <- list(success = FALSE)
                             }
                           }
                           saveRDS(condition_reps, file = paste0("cond_", cond, ".rds"))
                           return(condition_reps) 
                         }
stopCluster(cl)
save(final_results, file = "study1_results.Rdata")
message("Simulation complete. Results saved.")