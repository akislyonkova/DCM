library(GDINA)
library(foreach)
library(doParallel)
library(doRNG)

getwd()
setwd("~/DCM/sism/")     
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)


data_dir <- "polytomous_SISM_sim"          

design_info <- readRDS(file.path(data_dir, "design_info.rds"))
cell_design <- design_info$design          # 64 rows: dgp cond x misspecification
skill_cols  <- design_info$skill_cols
bug_cols    <- design_info$bug_cols
attr_cols   <- c(skill_cols, bug_cols)
K.skills    <- length(skill_cols)
no.bugs     <- length(bug_cols)
n_conditions <- nrow(cell_design)
n_reps       <- design_info$n_rep
stopifnot(n_conditions == 64, n_reps == 100)


cell_files <- file.path(data_dir, sprintf(
  "cell%02d_dist-%s_ncat-%d_loc-%s_step-%s_attr-%s_err-%s.rds",
  cell_design$cell, cell_design$dist, cell_design$ncat, cell_design$loc,
  cell_design$step, cell_design$Attribute_Type, cell_design$Error_Type))
stopifnot(all(file.exists(cell_files)))
cell_label <- sub("\\.rds$", "", basename(cell_files))

expand_steps <- function(dat, Qc) {
  dat <- as.matrix(dat)
  out <- sapply(seq_len(nrow(Qc)), function(r) {
    s <- dat[, Qc$Item[r]]; h <- Qc$Cat[r]
    ifelse(s >= h, 1L, ifelse(s == h - 1, 0L, NA_integer_))
  })
  colnames(out) <- paste0("Item", Qc$Item, "_Cat", Qc$Cat)
  out
}

custom_sism <- function(Qrow, K.skills, no.bugs) {
  qrow      <- as.numeric(Qrow)
  skill_pos <- which(qrow[1:K.skills] == 1)
  bug_pos   <- K.skills + which(qrow[(K.skills + 1):(K.skills + no.bugs)] == 1)
  req_pos   <- sort(c(skill_pos, bug_pos))
  is_skill  <- req_pos %in% skill_pos
  has_skill <- length(skill_pos) > 0
  has_bug   <- length(bug_pos)   > 0
  
  patt    <- attributepattern(length(req_pos))
  master  <- apply(patt[, is_skill,  drop = FALSE] == 1, 1, all)
  bugfree <- apply(patt[, !is_skill, drop = FALSE] == 0, 1, all)   # bug attribute 1 = present
  
  D <- cbind(intercept = rep(1, nrow(patt)))
  if (has_skill) D <- cbind(D, skill = as.numeric(master))
  if (has_bug)   D <- cbind(D, bug   = as.numeric(bugfree))
  if (has_skill && has_bug) D <- cbind(D, inter = as.numeric(master & bugfree))
  D
}



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


for (i in seq_len(n_conditions)) {
  cell_i <- readRDS(cell_files[i])
  d1     <- cell_i$reps[[1]]$dat
  check_polytomous_data(d1, cell_i$Qc_true)
  if (length(find_degenerate_steps(expand_steps(d1, cell_i$Qc_true), cell_i$Qc_true)) > 0) {
    warning(sprintf("Degenerate steps in %s, rep 1 -- inspect before running the full batch.",
                    cell_label[i]))
  }
  rm(cell_i, d1)
}


set.seed(2026)

final_results <- foreach(cond = 1:n_conditions,
                         .packages = "GDINA",
                         .export = c("cell_files", "cell_design", "cell_label", "custom_sism",
                                     "n_reps", "K.skills", "no.bugs", "skill_cols", "bug_cols", "attr_cols")) %dorng% {
                                       
                                       cell_dat       <- readRDS(cell_files[cond])      
                                       Qc             <- cell_dat$Qc_true
                                       condition_reps <- vector("list", n_reps)
                                       
                                       for (rep in 1:n_reps) {
                                         
                                         entry        <- cell_dat$reps[[rep]]
                                         current_data <- as.data.frame(entry$dat)          
                                         current_Q    <- cbind(Qc[, c("Item", "Cat")], entry$Q_mis$Q)   
                                         true_attr    <- as.matrix(entry$attribute)
                                         if (is.null(colnames(true_attr))) colnames(true_attr) <- attr_cols
                                         
                                         n_steps <- nrow(current_Q)
                                         D_list  <- lapply(seq_len(n_steps), function(r)
                                           custom_sism(current_Q[r, attr_cols], K.skills, no.bugs))
                                         
                                         fit_attempt <- tryCatch({
                                           GDINA(dat = current_data, Q = current_Q,
                                                 model         = rep("UDF", n_steps),
                                                 sequential    = TRUE,
                                                 design.matrix = D_list,
                                                 linkfunc      = rep("identity", n_steps),
                                                 verbose = 0,
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
                                             n_flips           = nrow(entry$Q_mis$flips),
                                             condition         = cell_design[cond, ],
                                             success           = TRUE
                                           )
                                           
                                         } else {
                                           condition_reps[[rep]] <- list(condition = cell_design[cond, ], success = FALSE)
                                         }
                                       }
                                       
                                       cond_label <- cell_label[cond]
                                       saveRDS(condition_reps, file = paste0("fit_", cond_label, ".rds"))
                                       message(sprintf("Condition %d/%d (%s) complete", cond, n_conditions, cond_label))
                                       
                                       condition_reps
                                     }

names(final_results) <- cell_label

stopCluster(cl)
save(final_results, file = "study3_polytomous_results.RData")
message("Study 3 simulation complete. Results saved.")