
#   pi00 = P(success | skills NOT mastered, bug(s) present)   worst case
#   pi10 = P(success | skills mastered,     bug(s) present)
#   pi01 = P(success | skills NOT mastered, bug(s) absent)
#   pi11 = P(success | skills mastered,     bug(s) absent)    best case


library(GDINA)


Qc <- data.frame(
  Item = c(1, 1, 2, 2, 3, 4, 5),
  Cat  = c(1, 2, 1, 2, 1, 1, 1),
  A1   = c(1, 1, 0, 0, 0, 1, 0),
  A2   = c(0, 0, 1, 1, 0, 1, 1),
  A3   = c(0, 0, 0, 0, 1, 0, 1),
  B1   = c(1, 0, 1, 0, 1, 0, 1),
  B2   = c(0, 1, 0, 1, 0, 1, 1)
)
no.bugs  <- 2
K.skills <- ncol(Qc) - 2 - no.bugs
Qexp     <- as.matrix(Qc[, -(1:2)])   # plain Q-matrix, 1 row per pseudo-item (step)
J        <- nrow(Qc)                   # total number of steps (here: 7)


set.seed(2026)
true_sism_probs <- data.frame(
  Item = Qc$Item, Cat = Qc$Cat,
  pi00 = round(runif(J, 0.05, 0.15), 3),   # neither skills nor bug-free
  pi10 = round(runif(J, 0.35, 0.50), 3),   # skills mastered, bug still present
  pi01 = round(runif(J, 0.35, 0.50), 3),   # skills not mastered, but bug-free
  pi11 = round(runif(J, 0.80, 0.95), 3)    # skills mastered AND bug-free (best)
)
print(true_sism_probs)
stopifnot(all(true_sism_probs$pi11 > pmax(true_sism_probs$pi10, true_sism_probs$pi01)),
          all(pmin(true_sism_probs$pi10, true_sism_probs$pi01) > true_sism_probs$pi00))


build_sism_catprob <- function(Qc, K.skills, no.bugs, true_probs) {
  catprob <- vector("list", nrow(Qc))
  for (r in seq_len(nrow(Qc))) {
    qrow <- as.numeric(Qc[r, -(1:2)])
    skill_pos <- which(qrow[1:K.skills] == 1)                       # required skill cols
    bug_pos   <- K.skills + which(qrow[(K.skills+1):(K.skills+no.bugs)] == 1)  # required bug cols
    req_pos   <- sort(c(skill_pos, bug_pos))     # preserves left-to-right column order
    is_skill  <- req_pos %in% skill_pos
    Kj <- length(req_pos)

    patt <- attributepattern(Kj)                 # 2^Kj x Kj, GDINA's own enumeration
    pi00 <- true_probs$pi00[r]; pi10 <- true_probs$pi10[r]
    pi01 <- true_probs$pi01[r]; pi11 <- true_probs$pi11[r]

    probs <- apply(patt, 1, function(bits) {
      skill_mastered <- all(bits[is_skill] == 1)
      bug_free       <- all(bits[!is_skill] == 0)
      if (skill_mastered && bug_free)      pi11
      else if (skill_mastered && !bug_free) pi10
      else if (!skill_mastered && bug_free) pi01
      else                                   pi00
    })
    catprob[[r]] <- probs
  }
  names(catprob) <- paste0("Item", Qc$Item, "_Cat", Qc$Cat)
  catprob
}

catprob_list <- build_sism_catprob(Qc, K.skills, no.bugs, true_sism_probs)


collapse_to_polytomous <- function(step_dat, Qc) {
  items <- unique(Qc$Item)
  N <- nrow(step_dat)
  poly <- matrix(NA_integer_, N, length(items)); colnames(poly) <- paste0("Item", items)
  exp_masked <- step_dat

  for (jx in seq_along(items)) {
    j <- items[jx]
    cats <- sort(Qc$Cat[Qc$Item == j])
    score <- rep(0L, N)
    reached <- rep(TRUE, N)
    for (h in cats) {
      col <- which(Qc$Item == j & Qc$Cat == h)
      resp <- step_dat[, col]          
      exp_masked[!reached, col] <- NA
      succeeded <- reached & (resp == 1)
      score[succeeded] <- h
      reached <- succeeded
    }
    poly[, jx] <- score
  }
  list(dat = as.data.frame(poly), dat_expanded = exp_masked)
}


simulate_one <- function(N, seed, Qc, Qexp, catprob_list) {
  set.seed(seed)
  sim_steps <- simGDINA(N = N, Q = Qexp, catprob.parm = catprob_list)
  step_dat  <- extract(sim_steps, "dat")
  collapsed <- collapse_to_polytomous(step_dat, Qc)

  list(
    dat          = collapsed$dat,
    dat_expanded = collapsed$dat_expanded,
    attribute    = extract(sim_steps, "attribute"),
    true_probs   = catprob_list,          
    seed         = seed,
    N            = N
  )
}


one <- simulate_one(N = 1000, seed = 12345, Qc = Qc, Qexp = Qexp, catprob_list = catprob_list)
dat            <- one$dat
dat_expanded   <- one$dat_expanded
true_attribute <- one$attribute


print(sapply(dat, function(x) var(as.numeric(x))))

print(lapply(dat, table))

print(colSums(!is.na(dat_expanded)))

for (col in seq_len(ncol(dat_expanded))) {
  vals <- unique(na.omit(dat_expanded[, col]))   
  if (length(vals) < 2) {
    warning(sprintf("Simulated Item %d Cat %d is degenerate (only value %s) at N = %d, seed = %d.",
                     Qc$Item[col], Qc$Cat[col], paste(vals, collapse=","), one$N, one$seed))
  }
}


master_seed <- 20260908
set.seed(master_seed)

sim_conditions <- expand.grid(
  N   = c(500, 1000, 2000),
  rep = 1:100,
  KEEP.OUT.ATTRS = FALSE
)
sim_conditions$seed <- sample(1e5:1e7, nrow(sim_conditions))

sim_data_list <- vector("list", nrow(sim_conditions))
for (i in seq_len(nrow(sim_conditions))) {
  sim_data_list[[i]] <- simulate_one(
    N = sim_conditions$N[i], seed = sim_conditions$seed[i],
    Qc = Qc, Qexp = Qexp, catprob_list = catprob_list
  )
}
names(sim_data_list) <- paste0("N", sim_conditions$N, "_rep", sim_conditions$rep)

saveRDS(
  list(master_seed = master_seed, Qc = Qc, Qexp = Qexp, no.bugs = no.bugs,
       true_sism_probs = true_sism_probs, conditions = sim_conditions,
       data = sim_data_list),
  file = "sequential_SISM_simulation_data.rds"
)
