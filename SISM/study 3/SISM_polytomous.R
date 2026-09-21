library(GDINA)

sim_out <- readRDS("sequential_SISM_simulation_data.rds")
Qc      <- sim_out$Qc
Qexp    <- sim_out$Qexp
no.bugs <- sim_out$no.bugs

# pick one condition, e.g. first N=1000 replication:
one_run <- sim_out$data[["N1000_rep1"]]
dat     <- one_run$dat



expand_to_stepwise <- function(dat, Qc) {
  items <- unique(Qc$Item)
  out <- matrix(NA_integer_, nrow = nrow(dat), ncol = nrow(Qc))
  colnames(out) <- paste0("Item", Qc$Item, "_Cat", Qc$Cat)
  for (j in items) {
    cats <- sort(Qc$Cat[Qc$Item == j])
    s <- dat[[j]]
    for (h in cats) {
      col <- which(Qc$Item == j & Qc$Cat == h)
      attempted <- s >= (h - 1)
      out[attempted, col]  <- as.integer(s[attempted] >= h)
      out[!attempted, col] <- NA
    }
  }
  as.data.frame(out)
}


dat_expanded <- expand_to_stepwise(dat, Qc)   # N x nrow(Qc) binary/NA matrix


check_polytomous_data <- function(dat, Qc, min_n_per_cat = 20) {
  for (j in unique(Qc$Item)) {
    max_cat <- max(Qc$Cat[Qc$Item == j])
    obs <- sort(unique(na.omit(dat[[j]])))
    if (!all(obs %in% 0:max_cat)) {
      warning(sprintf("Item %d: response codes %s fall outside expected range 0:%d.",
                       j, paste(obs, collapse = ","), max_cat))
    }
    if (any(table(dat[[j]]) < min_n_per_cat)) {
      warning(sprintf("Item %d: a score category has fewer than %d respondents -- SISM's
delta_j0/delta_j1/delta_j2/delta_j12 for that step may be unstable.", j, min_n_per_cat))
    }
  }
}
check_polytomous_data(dat, Qc)


find_degenerate_steps <- function(dat_expanded, Qc) {
  bad <- integer(0)
  for (col in seq_len(ncol(dat_expanded))) {
    vals <- unique(na.omit(dat_expanded[[col]]))
    n_nonmissing <- sum(!is.na(dat_expanded[[col]]))
    if (length(vals) < 2) {
      bad <- c(bad, col)
      cat(sprintf(
        "DEGENERATE: Item %d, Cat %d (column '%s') has only value(s) {%s} across %d non-missing respondents.\n",
        Qc$Item[col], Qc$Cat[col], colnames(dat_expanded)[col],
        paste(vals, collapse = ","), n_nonmissing))
    }
  }
  if (length(bad) == 0) cat("No degenerate steps found -- safe to fit.\n")
  invisible(bad)
}
degenerate_cols <- find_degenerate_steps(dat_expanded, Qc)


stopifnot("Resolve degenerate steps before fitting -- see messages above." =
            length(degenerate_cols) == 0)


mod_seqSISM <- GDINA(
  dat             = dat_expanded,
  Q               = Qexp,
  model           = "SISM",
  sequential      = FALSE,                # expansion above already IS the "sequential" step
  no.bugs         = no.bugs,
  control         = list(nstarts = 5,     # multiple random starts -> avoid local optima
                          conv.crit = 1e-5,
                          conv.type = c("ip","mp"))
)

summary(mod_seqSISM)


monocheck(mod_seqSISM)


delta_est <- coef(mod_seqSISM, what = "delta")
print(delta_est)


n_par <- npar(mod_seqSISM)
n_obs <- sum(!is.na(dat_expanded[[1]])) + 0  # per-step sample sizes vary; report total N
n_obs <- nrow(dat_expanded)


colSums(!is.na(dat_expanded))


modelfit(mod_seqSISM)
itemfit(mod_seqSISM)


mod_seqBUGDINO <- GDINA(dat_expanded, Qexp, model = "BUGDINO", no.bugs = no.bugs)
anova(mod_seqSISM, mod_seqBUGDINO)
