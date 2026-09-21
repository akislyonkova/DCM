library(GDINA)
library(foreach)
library(doParallel)
library(doRNG)

getwd()
setwd("~/DCM/sism/")     
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)


sim_out    <- readRDS("polytomous_SISM_data.rds")  
Qc         <- sim_out$Qc
Qexp       <- sim_out$Qexp
no.bugs    <- sim_out$no.bugs
K.skills   <- sim_out$K.skills
skill_cols <- sim_out$skill_cols
bug_cols   <- sim_out$bug_cols


check_polytomous_data <- function(dat, Qc, min_n_per_cat = 20) {
  for (j in unique(Qc$Item)) {
    max_cat <- max(Qc$Cat[Qc$Item == j])
    obs <- sort(unique(na.omit(dat[[j]])))
    if (!all(obs %in% 0:max_cat)) {
      warning(sprintf("Item %d: response codes %s fall outside expected range 0:%d.",
                      j, paste(obs, collapse = ","), max_cat))
    }
    if (any(table(dat[[j]]) < min_n_per_cat)) {
      warning(sprintf("Item %d: a score category has fewer than %d respondents.", j, min_n_per_cat))
    }
  }
}

find_degenerate_steps <- function(dat_expanded, Qc) {
  bad <- integer(0)
  for (col in seq_len(ncol(dat_expanded))) {
    vals <- unique(na.omit(dat_expanded[[col]]))
    if (length(vals) < 2) bad <- c(bad, col)
  }
  bad
}


check_polytomous_data(sim_out$data[[1]]$dat, Qc)
if (length(find_degenerate_steps(sim_out$data[[1]]$dat_expanded, Qc)) > 0) {
  warning("Degenerate steps found in sim_out$data[[1]] -- inspect before running the full batch.")
}


cond_key <- unique(sim_out$conditions[, c("Attribute_Type", "Error_Type", "Error_Rate",
                                          "N", "Item_Quality")])
cond_key <- cond_key[do.call(order, cond_key), ]
rownames(cond_key) <- NULL
n_conditions <- nrow(cond_key)
n_reps       <- max(sim_out$conditions$rep)
stopifnot(n_conditions == 32, n_reps == 100)

idx_matrix <- matrix(NA_integer_, nrow = n_conditions, ncol = n_reps)
for (cc in seq_len(n_conditions)) {
  for (rr in seq_len(n_reps)) {
    match_idx <- with(sim_out$conditions, which(
      Attribute_Type == cond_key$Attribute_Type[cc] &
        Error_Type     == cond_key$Error_Type[cc] &
        Error_Rate     == cond_key$Error_Rate[cc] &
        N              == cond_key$N[cc] &
        Item_Quality   == cond_key$Item_Quality[cc] &
        rep            == rr
    ))
    stopifnot(length(match_idx) == 1)
    idx_matrix[cc, rr] <- match_idx
  }
}


set.seed(2026)

fit_comparison_model <- TRUE   # also fits BUGDINO and runs anova() against it, for
# the "which model is statistically preferred" IC
# comparison in the study design -- roughly doubles
# runtime; set FALSE if the server run needs to be faster

final_results <- foreach(cond = 1:n_conditions,
                         .packages = "GDINA",
                         .export = c("sim_out", "idx_matrix", "cond_key",
                                     "n_reps", "no.bugs", "skill_cols", "bug_cols",
                                     "fit_comparison_model")) %dorng% {
                                       
                                       condition_reps <- vector("list", n_reps)
                                       
                                       for (rep in 1:n_reps) {
                                         
                                         entry        <- sim_out$data[[ idx_matrix[cond, rep] ]]
                                         current_data <- entry$dat_expanded
                                         current_Q    <- entry$Q_misspecified     # <- the flawed Q-matrix that defines this condition
                                         true_attr    <- as.matrix(entry$true_attribute)
                                         if (is.null(colnames(true_attr))) colnames(true_attr) <- colnames(Qexp)
                                         
                                         fit_attempt <- tryCatch({
                                           GDINA(dat = current_data, Q = current_Q, model = "SISM",
                                                 sequential = FALSE, no.bugs = no.bugs, verbose = 0,
                                                 control = list(nstarts = 5, conv.crit = 1e-5, conv.type = c("ip", "mp")))
                                         }, error = function(e) {
                                           warning(sprintf("GDINA fit failed -- cond %d, rep %d: %s", cond, rep, e$message))
                                           return(NULL)
                                         })
                                         
                                         if (!is.null(fit_attempt)) {
                                           
                                           estimates  <- tryCatch(coef(fit_attempt, what = "delta"),
                                                                  error = function(e) { warning(sprintf("coef() failed -- cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                           
                                           fit_stats  <- tryCatch(modelfit(fit_attempt),
                                                                  error = function(e) { warning(sprintf("modelfit() failed -- cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                           
                                           person_mp  <- tryCatch(personparm(fit_attempt, what = "mp"),
                                                                  error = function(e) { warning(sprintf("personparm(mp) failed -- cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                           
                                           class_prev <- tryCatch(coef(fit_attempt, what = "lambda"),
                                                                  error = function(e) { warning(sprintf("coef(lambda) failed -- cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                           
                                           profiles   <- tryCatch(personparm(fit_attempt, what = "MAP"),
                                                                  error = function(e) { warning(sprintf("personparm(MAP) failed -- cond %d, rep %d: %s", cond, rep, e$message)); NULL })
                                           
                                           ccr_overall <- ccr_pattern <- ccr_skill <- ccr_misconception <- NA
                                           if (!is.null(profiles)) {
                                             est_mat <- as.matrix(profiles)[, seq_len(ncol(true_attr)), drop = FALSE]
                                             colnames(est_mat) <- colnames(true_attr)
                                             match_mat <- (est_mat == true_attr)
                                             ccr_overall       <- mean(match_mat)
                                             ccr_pattern        <- mean(rowSums(match_mat) == ncol(match_mat))
                                             ccr_skill          <- mean(match_mat[, colnames(true_attr) %in% skill_cols, drop = FALSE])
                                             ccr_misconception  <- mean(match_mat[, colnames(true_attr) %in% bug_cols,   drop = FALSE])
                                           }
                                           
                                           comparison <- NULL
                                           if (fit_comparison_model) {
                                             comparison <- tryCatch({
                                               mod_comp <- GDINA(current_data, current_Q, model = "BUGDINO",
                                                                 no.bugs = no.bugs, verbose = 0)
                                               anova(fit_attempt, mod_comp)
                                             }, error = function(e) {
                                               warning(sprintf("BUGDINO comparison failed -- cond %d, rep %d: %s", cond, rep, e$message))
                                               NULL
                                             })
                                           }
                                           
                                           condition_reps[[rep]] <- list(
                                             estimates         = estimates,
                                             fit_stats         = fit_stats,
                                             person_mp         = person_mp,
                                             class_prev        = class_prev,
                                             profiles          = profiles,
                                             ccr_overall       = ccr_overall,
                                             ccr_pattern       = ccr_pattern,
                                             ccr_skill         = ccr_skill,
                                             ccr_misconception = ccr_misconception,
                                             comparison        = comparison,
                                             n_flips           = entry$condition$n_flips,
                                             condition         = cond_key[cond, ],
                                             success           = TRUE
                                           )
                                           
                                         } else {
                                           condition_reps[[rep]] <- list(condition = cond_key[cond, ], success = FALSE)
                                         }
                                       }
                                       
                                       cond_label <- paste0(
                                         "Attr-", cond_key$Attribute_Type[cond], "_Err-", cond_key$Error_Type[cond],
                                         "_Rate-", cond_key$Error_Rate[cond] * 100, "_N-", cond_key$N[cond],
                                         "_Qual-", cond_key$Item_Quality[cond]
                                       )
                                       saveRDS(condition_reps, file = paste0("cond_", cond, "_", cond_label, ".rds"))
                                       message(sprintf("Condition %d/%d (%s) complete", cond, n_conditions, cond_label))
                                       
                                       condition_reps
                                     }

names(final_results) <- paste0(
  "Attr-", cond_key$Attribute_Type, "_Err-", cond_key$Error_Type,
  "_Rate-", cond_key$Error_Rate * 100, "_N-", cond_key$N,
  "_Qual-", cond_key$Item_Quality
)

stopCluster(cl)
save(final_results, file = "study3_results.RData")
message("Study 3 simulation complete. Results saved.")
