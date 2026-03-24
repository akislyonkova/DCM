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
load("/home/akislyonkova/DCM/sism/Study1_data.RData")

set.seed(2026)

n_conditions <- 64
n_reps <- 100

final_results <- foreach(cond = 1:n_conditions, 
                         .packages = "GDINA",
                         .export = c("data", "n_reps")) %dorng% {
                           
                           condition_reps <- vector("list", n_reps);
                           
                           for (rep in 1:n_reps) {
                             current_Q <- data[[cond]]$replications[[rep]]$Q_used
                             current_data <- data[[cond]]$replications[[rep]]$dat
                             
                             fit_attempt <- tryCatch({
                               GDINA(dat = current_data, 
                                     Q = current_Q, 
                                     model = "SISM", 
                                     no.bugs = 3, 
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
save(final_results, file = "study1_results.Rdata")
message("Simulation complete. Results saved.")