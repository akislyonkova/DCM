library(GDINA)


master_seed <- 20260908
N           <- 2000          
n_rep       <- 100
n_blocks    <- 4             # for J =20
disc_skill  <- 1.5             
disc_bug    <- 1.5             
loc_val     <- c(neg = -0.75, pos = 0.75)   
step_val    <- c(small = 0.4, large = 1.2)  
skew_r      <- 0.7                          # geometric ratio for skewed target
error_rate  <- 0.10          
out_dir     <- "polytomous_SISM_sim"
dir.create(out_dir, showWarnings = FALSE)


dgp_design <- expand.grid(
  dist = c("skew", "flat"),
  ncat = c(3, 5),
  loc  = c("neg", "pos"),
  step = c("small", "large"),
  stringsAsFactors = FALSE
)
dgp_design$cond <- seq_len(nrow(dgp_design))

misspec_design <- expand.grid(
  Attribute_Type = c("skill", "misconception"),
  Error_Type     = c("omission", "inclusion"),
  stringsAsFactors = FALSE
)
misspec_design$misspec <- seq_len(nrow(misspec_design))

# Full 64-cell grid (cross of the two designs)
full_design <- merge(dgp_design, misspec_design, by = NULL)
full_design <- full_design[order(full_design$cond, full_design$misspec), ]
full_design$cell <- seq_len(nrow(full_design))


no_skills <- 3; no_bugs <- 2
skill_cols <- paste0("A", 1:no_skills)
bug_cols   <- paste0("B", 1:no_bugs)
attr_cols  <- c(skill_cols, bug_cols)

Q_base <- matrix(c(            #  A1 A2 A3 B1 B2
  1, 0, 0, 1, 0,              # item 1
  0, 1, 0, 0, 1,              # item 2
  0, 0, 1, 1, 0,              # item 3
  1, 1, 0, 0, 1,              # item 4
  0, 1, 1, 1, 1               # item 5
), ncol = 5, byrow = TRUE, dimnames = list(NULL, attr_cols))
Qitem <- Q_base[rep(seq_len(nrow(Q_base)), n_blocks), , drop = FALSE]
J     <- nrow(Qitem)


set.seed(master_seed)
item_shift <- rnorm(J, 0, 0.2)


expand_Q <- function(Qitem, ncat) {
  m   <- ncat - 1
  idx <- rep(seq_len(nrow(Qitem)), each = m)
  data.frame(Item = idx, Cat = rep(seq_len(m), times = nrow(Qitem)),
             Qitem[idx, , drop = FALSE], row.names = NULL)
}


pass_prob <- function(eta, skill_mastered, bug_free) {
  plogis(eta + (disc_skill * (2 * skill_mastered - 1) +
                disc_bug   * (2 * bug_free       - 1)) / 2)
}

step_eta <- function(ncat, loc, step) {         # J x (ncat-1) matrix of pass logits
  m     <- ncat - 1
  k_off <- (seq_len(m) - mean(seq_len(m))) * step_val[[step]]
  -(loc_val[[loc]] + outer(item_shift, k_off, "+"))
}


build_sism_catprob <- function(Qc, eta) {
  Qatt <- as.matrix(Qc[, attr_cols])
  lapply(seq_len(nrow(Qc)), function(r) {
    req      <- which(Qatt[r, ] == 1)
    is_skill <- req <= no_skills
    patt     <- attributepattern(length(req))
    sm <- rowSums(patt[,  is_skill, drop = FALSE]) == sum(is_skill)
    bf <- rowSums(patt[, !is_skill, drop = FALSE]) == 0     # bug attribute 1 = present
    pass_prob(eta[Qc$Item[r], Qc$Cat[r]], sm, bf)
  })
}


target_props <- function(dist, ncat) {
  p <- if (dist == "flat") rep(1, ncat) else skew_r^(0:(ncat - 1))
  p / sum(p)
}

cat_probs <- function(p) {                       # category probs from step pass probs
  c(1, cumprod(p)) * c(1 - p, 1)
}

expected_props <- function(pi, eta) {
  n_s <- rowSums(Qitem[, skill_cols, drop = FALSE])
  n_b <- rowSums(Qitem[, bug_cols,   drop = FALSE])
  props <- sapply(seq_len(J), function(j) {
    ps <- pi^n_s[j]; pb <- pi^n_b[j]
    w  <- c(both = ps * pb, skill_only = ps * (1 - pb),
            bug_free_only = (1 - ps) * pb, neither = (1 - ps) * (1 - pb))
    st <- list(c(1, 1), c(1, 0), c(0, 1), c(0, 0))   # (skill mastered, bug free)
    Reduce(`+`, Map(function(wi, s) wi * cat_probs(pass_prob(eta[j, ], s[1], s[2])),
                    w, st))
  })
  rowMeans(props)
}

calibrate_prev <- function(eta, target) {     # makes the model-implied category proportions as close as possible to the target
  f <- function(pi) sum((expected_props(pi, eta) - target)^2)
  pi_hat <- optimize(f, c(0.02, 0.98))$minimum
  list(pi = pi_hat, expected = expected_props(pi_hat, eta))
}

attribute_prior <- function(pi) {
  pats <- attributepattern(no_skills + no_bugs)
  apply(pats, 1, function(a) {
    s <- a[1:no_skills]; b <- a[(no_skills + 1):(no_skills + no_bugs)]
    prod(ifelse(s == 1, pi, 1 - pi)) * prod(ifelse(b == 1, 1 - pi, pi))
  })
}


collapse_to_polytomous <- function(step_dat, Qc) {
  items <- unique(Qc$Item)
  poly  <- matrix(0L, nrow(step_dat), length(items),
                  dimnames = list(NULL, paste0("Item", items)))
  for (j in items) {
    reached <- rep(TRUE, nrow(step_dat))
    for (h in sort(Qc$Cat[Qc$Item == j])) {
      col     <- which(Qc$Item == j & Qc$Cat == h)
      success <- reached & (step_dat[, col] == 1)
      poly[success, j] <- h
      reached <- success
    }
  }
  poly
}

expand_steps <- function(dat, Qc) {
  dat <- as.matrix(dat)
  out <- sapply(seq_len(nrow(Qc)), function(r) {
    s <- dat[, Qc$Item[r]]; h <- Qc$Cat[r]
    ifelse(s >= h, 1L, ifelse(s == h - 1, 0L, NA_integer_))
  })
  colnames(out) <- paste0("Item", Qc$Item, "_Cat", Qc$Cat)
  out
}

simulate_one <- function(N, seed, Qc, catprob, prior) {
  set.seed(seed)
  Qexp <- as.matrix(Qc[, attr_cols])
  sim  <- simGDINA(N, Qexp, catprob.parm = catprob,
                   att.dist = "categorical", att.prior = prior)
  list(dat       = collapse_to_polytomous(extract(sim, "dat"), Qc),
       attribute = extract(sim, "attribute"))
}


misspecify_Q <- function(Qc, attribute_type, error_type, error_rate, seed) {
  set.seed(seed)
  Qatt        <- as.matrix(Qc[, attr_cols])
  target_cols <- if (attribute_type == "skill") skill_cols else bug_cols
  target_val  <- if (error_type == "omission") 1 else 0
  
  cells  <- which(Qatt[, target_cols, drop = FALSE] == target_val, arr.ind = TRUE)
  n_flip <- max(1, round(error_rate * nrow(cells)))
  pick   <- cells[sample(nrow(cells), n_flip), , drop = FALSE]
  col_ix <- match(target_cols[pick[, 2]], attr_cols)
  
  Qmis <- Qatt
  Qmis[cbind(pick[, 1], col_ix)] <- 1 - target_val
  stopifnot(all(rowSums(Qmis) > 0))               # no empty Q-matrix rows
  
  list(Q = Qmis,
       flips = data.frame(Item = Qc$Item[pick[, 1]], Cat = Qc$Cat[pick[, 1]],
                          Column = attr_cols[col_ix],
                          From = target_val, To = 1 - target_val))
}


set.seed(master_seed)
n_draw    <- nrow(full_design) * n_rep
seed_pool <- sample(1e5:1e7, 2 * n_draw)
data_seed <- matrix(seed_pool[1:n_draw],                  nrow(full_design), n_rep)
qmis_seed <- matrix(seed_pool[(n_draw + 1):(2 * n_draw)], nrow(full_design), n_rep)


setup_dgp <- function(cr) {
  Qc  <- expand_Q(Qitem, cr$ncat)
  eta <- step_eta(cr$ncat, cr$loc, cr$step)
  target <- target_props(cr$dist, cr$ncat)
  cal    <- calibrate_prev(eta, target)
  list(Qc = Qc, eta = eta, target = target, pi = cal$pi, expected = cal$expected,
       catprob = build_sism_catprob(Qc, eta), prior = attribute_prior(cal$pi))
}
dgp_setup <- lapply(seq_len(nrow(dgp_design)), function(i) setup_dgp(dgp_design[i, ]))


check <- vector("list", nrow(full_design))

for (i in seq_len(nrow(full_design))) {
  cr <- full_design[i, ]
  su <- dgp_setup[[cr$cond]]
  
  reps <- lapply(seq_len(n_rep), function(r) {
    d <- simulate_one(N, data_seed[i, r], su$Qc, su$catprob, su$prior)
    d$data_seed <- data_seed[i, r]
    d$Q_mis     <- misspecify_Q(su$Qc, cr$Attribute_Type, cr$Error_Type,
                                error_rate, qmis_seed[i, r])
    d$qmis_seed <- qmis_seed[i, r]
    d
  })
  
  # calibration + degenerate-category checks
  obs <- prop.table(table(factor(unlist(reps[[1]]$dat), levels = 0:(cr$ncat - 1))))
  missing_cat <- sum(sapply(reps, function(x)
    any(apply(x$dat, 2, function(v) length(unique(v)) < cr$ncat))))
  if (missing_cat > 0)
    warning(sprintf("Cell %d: %d/%d reps have an item with an unobserved category.",
                    i, missing_cat, n_rep))
  check[[i]] <- data.frame(cr, prev = round(su$pi, 3),
                           max_dev_expected = round(max(abs(su$expected - su$target)), 3),
                           max_dev_observed = round(max(abs(as.numeric(obs) - su$target)), 3),
                           reps_with_missing_cat = missing_cat)
  
  saveRDS(list(condition = cr, Qc_true = su$Qc, eta = su$eta, catprob = su$catprob,
               pi = su$pi, target = su$target, expected = su$expected,
               N = N, error_rate = error_rate, reps = reps),
          file = file.path(out_dir, sprintf(
            "cell%02d_dist-%s_ncat-%d_loc-%s_step-%s_attr-%s_err-%s.rds",
            cr$cell, cr$dist, cr$ncat, cr$loc, cr$step,
            cr$Attribute_Type, cr$Error_Type)))
  message(sprintf("Saved cell %d / %d", i, nrow(full_design)))
}

check <- do.call(rbind, check)
print(check)

saveRDS(list(master_seed = master_seed, N = N, n_rep = n_rep, J = J,
             error_rate = error_rate, Qitem = Qitem, item_shift = item_shift,
             skill_cols = skill_cols, bug_cols = bug_cols,
             design = full_design, data_seed = data_seed, qmis_seed = qmis_seed,
             calibration_check = check),
        file = file.path(out_dir, "design_info.rds"))
