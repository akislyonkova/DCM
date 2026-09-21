#Study 3: Errors in the measurement component -- polytomous study

#   Attribute Type : Skill columns (A1-A3)  vs. Misconception columns (B1-B2)
#   Error Type     : Omission (true q_jk=1 miscoded as 0)
#                    Inclusion (true q_jk=0 miscoded as 1)
#   Error Rate     : 5% vs. 15% of the targeted Q-matrix cells flipped
#   Sample Size    : N = 500 vs. 1000
#   Item Quality   : High (s=g=0.10) vs. Low (s=g=0.25)


#   pi00 = P(success | skills NOT mastered, bug(s) present)   worst case
#   pi10 = P(success | skills mastered,     bug(s) present)
#   pi01 = P(success | skills NOT mastered, bug(s) absent)
#   pi11 = P(success | skills mastered,     bug(s) absent)    best case


library(GDINA)

#1. True Q-matrix
Qc <- data.frame(
  Item = c(1, 1, 2, 2, 3, 4, 5),
  Cat  = c(1, 2, 1, 2, 1, 1, 1),
  A1   = c(1, 1, 0, 0, 0, 1, 0),
  A2   = c(0, 0, 1, 1, 0, 1, 1),
  A3   = c(0, 0, 0, 0, 1, 0, 1),
  B1   = c(1, 0, 1, 0, 1, 0, 1),
  B2   = c(0, 1, 0, 1, 0, 1, 1)
)
no.bugs    <- 2
K.skills   <- ncol(Qc) - 2 - no.bugs
skill_cols <- colnames(Qc)[3:(2 + K.skills)]                          # "A1" "A2" "A3"
bug_cols   <- colnames(Qc)[(3 + K.skills):(2 + K.skills + no.bugs)]   # "B1" "B2"
Qexp       <- as.matrix(Qc[, -(1:2)])   # plain Q-matrix, 1 row per pseudo-item (step)
J          <- nrow(Qc)                  # total number of steps (here: 7)

#eligible-cell counts per targeted column set 
# skill columns (A1-A3): 9 ones/12 zeros  (21 cells)
# misconception columns (B1-B2): 8 ones/6 zeros  (14 cells)

set.seed(2026)
gen_true_sism_probs <- function(J, Qc, quality = c("high", "low"), seed) {
  quality <- match.arg(quality)
  set.seed(seed)
  
  if (quality == "high") {           # s = g = 0.10
    g <- 0.10; s <- 0.10
  } else {                            # s = g = 0.25
    g <- 0.25; s <- 0.25
  }
  
  pi00 <- round(runif(J, g - 0.05, g + 0.05), 3) # neither skills nor bug-free
  pi11 <- round(runif(J, (1 - s) - 0.05, (1 - s) + 0.05), 3) # skills mastered, bug still present
  pi10 <- round(runif(J, 0.35, 0.50), 3) # skills not mastered, but bug-free
  pi01 <- round(runif(J, 0.35, 0.50), 3) # skills mastered AND bug-free (best)
  
  true_probs <- data.frame(
    Item = Qc$Item, Cat = Qc$Cat,
    pi00 = pi00, pi10 = pi10, pi01 = pi01, pi11 = pi11
  )
  
  stopifnot(
    all(true_probs$pi11 > pmax(true_probs$pi10, true_probs$pi01)),
    all(pmin(true_probs$pi10, true_probs$pi01) > true_probs$pi00)
  )
  true_probs
}

build_sism_catprob <- function(Qc, K.skills, no.bugs, true_probs) {
  catprob <- vector("list", nrow(Qc))
  for (r in seq_len(nrow(Qc))) {
    qrow <- as.numeric(Qc[r, -(1:2)])
    skill_pos <- which(qrow[1:K.skills] == 1)                                     # required skill cols
    bug_pos   <- K.skills + which(qrow[(K.skills + 1):(K.skills + no.bugs)] == 1) # required bug cols
    req_pos   <- sort(c(skill_pos, bug_pos))                                      # preserves left-to-right column order
    is_skill  <- req_pos %in% skill_pos
    Kj <- length(req_pos)
    
    patt <- attributepattern(Kj)                                                  # 2^Kj x Kj, GDINA's own enumeration
    pi00 <- true_probs$pi00[r]; pi10 <- true_probs$pi10[r]
    pi01 <- true_probs$pi01[r]; pi11 <- true_probs$pi11[r]
    
    probs <- apply(patt, 1, function(bits) {
      skill_mastered <- all(bits[is_skill] == 1)
      bug_free       <- all(bits[!is_skill] == 0)
      if (skill_mastered && bug_free)       pi11
      else if (skill_mastered && !bug_free) pi10
      else if (!skill_mastered && bug_free) pi01
      else                                  pi00
    })
    catprob[[r]] <- probs
  }
  names(catprob) <- paste0("Item", Qc$Item, "_Cat", Qc$Cat)
  catprob
}



# catprob_list <- build_sism_catprob(Qc, K.skills, no.bugs, true_sism_probs)


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
    seed         = seed,
    N            = N
  )
}



misspecify_Q <- function(Qc, skill_cols, bug_cols,
                         attribute_type = c("skill", "misconception"),
                         error_type     = c("omission", "inclusion"),
                         error_rate, seed) {
  attribute_type <- match.arg(attribute_type)
  error_type     <- match.arg(error_type)
  set.seed(seed)
  
  Qmis <- Qc
  target_cols <- if (attribute_type == "skill") skill_cols else bug_cols
  target_val  <- if (error_type == "omission") 1 else 0    # value that gets miscoded
  flip_to     <- 1 - target_val
  
  flips <- data.frame(Item = integer(0), Cat = integer(0), Column = character(0),
                      From = integer(0), To = integer(0))
  
  for (col in target_cols) {
    eligible_rows <- which(Qmis[[col]] == target_val)
    if (length(eligible_rows) == 0) next
    flip_rows <- eligible_rows[runif(length(eligible_rows)) < error_rate]
    if (length(flip_rows) > 0) {
      flips <- rbind(flips, data.frame(
        Item = Qmis$Item[flip_rows], Cat = Qmis$Cat[flip_rows],
        Column = col, From = target_val, To = flip_to
      ))
      Qmis[flip_rows, col] <- flip_to
    }
  }
  
  list(Q = Qmis, flips = flips,
       attribute_type = attribute_type, error_type = error_type, error_rate = error_rate)
}

demo_true_probs <- gen_true_sism_probs(J, Qc, quality = "high", seed = 111)
demo_catprob    <- build_sism_catprob(Qc, K.skills, no.bugs, demo_true_probs)
demo_data       <- simulate_one(N = 1000, seed = 12345, Qc = Qc, Qexp = Qexp,
                                catprob_list = demo_catprob)
demo_qmis       <- misspecify_Q(Qc, skill_cols, bug_cols,
                                attribute_type = "skill", error_type = "omission",
                                error_rate = 0.15, seed = 999)

print(demo_true_probs)
print(sapply(demo_data$dat, function(x) var(as.numeric(x))))
print(demo_qmis$flips)   # which true 1's got miscoded as 0 in this draw

for (col in seq_len(ncol(demo_data$dat_expanded))) {
  vals <- unique(na.omit(demo_data$dat_expanded[, col]))
  if (length(vals) < 2) {
    warning(sprintf("Demo Item %d Cat %d is degenerate (only value %s).",
                    Qc$Item[col], Qc$Cat[col], paste(vals, collapse = ",")))
  }
}






master_seed <- 20260908
set.seed(master_seed)

sim_conditions <- expand.grid(
  Attribute_Type = c("skill", "misconception"),
  Error_Type     = c("omission", "inclusion"),
  Error_Rate     = c(0.05, 0.15),
  N              = c(500, 1000),
  Item_Quality   = c("high", "low"),
  rep            = 1:100,
  KEEP.OUT.ATTRS   = FALSE,
  stringsAsFactors = FALSE
)
stopifnot(nrow(sim_conditions) == 3200)

# independent seeds per dataset: one for the true response draw, one for
# the Q-matrix misspecification draw
sim_conditions$data_seed <- sample(1e5:1e7, nrow(sim_conditions))
sim_conditions$qmis_seed <- sample(1e5:1e7, nrow(sim_conditions))

# true item parameters: one fixed set per item quality level, shared across all reps/conditions of that quality level 
param_seed <- c(high = 111, low = 222)
true_probs_by_quality <- lapply(c(high = "high", low = "low"), function(q) {
  gen_true_sism_probs(J, Qc, quality = q, seed = param_seed[[q]])
})
catprob_by_quality <- lapply(names(true_probs_by_quality), function(q) {
  build_sism_catprob(Qc, K.skills, no.bugs, true_probs_by_quality[[q]])
})
names(catprob_by_quality) <- names(true_probs_by_quality)

sim_data_list <- vector("list", nrow(sim_conditions))
n_flips       <- integer(nrow(sim_conditions))

for (i in seq_len(nrow(sim_conditions))) {
  cond <- sim_conditions[i, ]
  
  # TRUE response data  generated from the TRUE Q-matrix
  true_dat <- simulate_one(
    N = cond$N, seed = cond$data_seed,
    Qc = Qc, Qexp = Qexp,
    catprob_list = catprob_by_quality[[cond$Item_Quality]]
  )
  
  # Misspecified Q-matrix 
  qmis <- misspecify_Q(
    Qc = Qc, skill_cols = skill_cols, bug_cols = bug_cols,
    attribute_type = cond$Attribute_Type, error_type = cond$Error_Type,
    error_rate = cond$Error_Rate, seed = cond$qmis_seed
  )
  n_flips[i] <- nrow(qmis$flips)
  
  # (c) degeneracy check on the true data
  for (col in seq_len(ncol(true_dat$dat_expanded))) {
    vals <- unique(na.omit(true_dat$dat_expanded[, col]))
    if (length(vals) < 2) {
      warning(sprintf(
        "Row %d (%s, N=%d, %s): Item %d Cat %d is degenerate at seed %d.",
        i, cond$Item_Quality, cond$N, cond$Attribute_Type,
        Qc$Item[col], Qc$Cat[col], cond$data_seed))
    }
  }
  
  sim_data_list[[i]] <- list(
    dat            = true_dat$dat,                    # polytomous item scores (N x 5)
    dat_expanded   = true_dat$dat_expanded,            # step-level 0/1 responses (masked)
    true_attribute = true_dat$attribute,               # true attribute profiles
    Q_true         = Qexp,                             # correct Q-matrix (steps x 5 attrs)
    Q_misspecified = as.matrix(qmis$Q[, -(1:2)]),       # Q-matrix WITH measurement error
    flips          = qmis$flips,                       # log of exactly which cells flipped
    condition      = cond
  )
}

sim_conditions$n_flips <- n_flips   # realized flips per row, vs. nominal error_rate

names(sim_data_list) <- with(sim_conditions, paste0(
  "Attr-", Attribute_Type, "_Err-", Error_Type, "_Rate-", Error_Rate * 100,
  "_N-", N, "_Qual-", Item_Quality, "_rep-", rep
))

saveRDS(
  list(
    master_seed            = master_seed,
    Qc                      = Qc,
    Qexp                    = Qexp,
    K.skills                = K.skills,
    no.bugs                 = no.bugs,
    skill_cols               = skill_cols,
    bug_cols                 = bug_cols,
    true_probs_by_quality    = true_probs_by_quality,
    conditions               = sim_conditions,
    data                     = sim_data_list
  ),
  file = "polytomous_SISM_data.rds"
)
