# =============================================================================
# Reproducible data generation: sequential SISM model, polytomous responses
# Companion to SISM_polytomous_GDINA.R -- Qc-matrix and no.bugs are IDENTICAL
# so the generated `dat` drops straight into the fitting script.
# =============================================================================
#
# simGDINA() natively supports this combination:
#   simGDINA(N, Q, gs.parm, model = "SISM", sequential = TRUE, no.bugs = <k>)
# gs.parm needs one row PER CATEGORY (nrow(Qc)), not per item -- guess =
# P(success | hasn't mastered skills / still has a bug), slip = 1 - P(success
# | mastered skills & bug-free). simGDINA derives the 4 SISM probabilities
# (epsilon, g, omega, h) from these internally.
# =============================================================================

library(GDINA)

## ---------------------------------------------------------------------------
## 1. Qc-matrix -- must match SISM_polytomous_GDINA.R exactly
## ---------------------------------------------------------------------------
Qc <- data.frame(
  Item = c(1,1, 2,2, 3, 4, 5),
  Cat  = c(1,2, 1,2, 1, 1, 1),
  A1   = c(1,1, 0,0, 0, 1, 0),
  A2   = c(0,0, 1,1, 0, 1, 1),
  A3   = c(0,0, 0,0, 1, 0, 1),
  B1   = c(1,0, 1,0, 1, 0, 1),
  B2   = c(0,1, 0,1, 0, 1, 1)
)
no.bugs <- 2
J <- nrow(Qc)                       # total sequential categories (here: 7)

## ---------------------------------------------------------------------------
## 2. True generating item parameters (guess/slip per category)
## ---------------------------------------------------------------------------
# Fixed across all replications -- this is what "true" values a recovery
# study checks estimates against. Draw them once, reproducibly, then reuse.
set.seed(2026)
gs_true <- data.frame(
  guess = round(runif(J, 0.05, 0.20), 3),   # P(success) for the "bad" latent group
  slip  = round(runif(J, 0.05, 0.20), 3)    # 1 - P(success) for the "good" latent group
)
rownames(gs_true) <- paste0("Item", Qc$Item, "_Cat", Qc$Cat)
print(gs_true)

## ---------------------------------------------------------------------------
## 3. Single-replication generator
## ---------------------------------------------------------------------------
simulate_one <- function(N, seed, Qc = Qc, gs = gs_true, no.bugs = no.bugs) {
  set.seed(seed)
  sim <- simGDINA(
    N          = N,
    Q          = Qc,
    gs.parm    = gs,
    model      = "SISM",
    sequential = TRUE,
    no.bugs    = no.bugs
  )
  list(
    dat          = extract(sim, "dat"),           # N x 5 polytomous item responses
    attribute    = extract(sim, "attribute"),      # N x (K.skills+no.bugs) true profiles
    catprob.parm = extract(sim, "catprob.parm"),   # true category success probs
    delta.parm   = extract(sim, "delta.parm"),     # true SISM delta params
    seed         = seed,
    N            = N
  )
}

## ---------------------------------------------------------------------------
## 4. Quick single dataset -- drop-in replacement for `dat` in the fitting
##    script (SISM_polytomous_GDINA.R). Use this while developing.
## ---------------------------------------------------------------------------
one <- simulate_one(N = 1000, seed = 12345, Qc = Qc, gs = gs_true, no.bugs = no.bugs)
dat            <- one$dat
true_attribute <- one$attribute

check_polytomous_data <- function(dat, Qc, min_n_per_cat = 20) {
  for (j in unique(Qc$Item)) {
    max_cat <- max(Qc$Cat[Qc$Item == j])
    tab <- table(dat[[j]])
    if (any(tab < min_n_per_cat)) {
      warning(sprintf("Item %d: a category has fewer than %d observations at N = %d.",
                      j, min_n_per_cat, nrow(dat)))
    }
  }
}
check_polytomous_data(dat, Qc)

## ---------------------------------------------------------------------------
## 5. Full simulation-study design: crossed N x replications, each with its
##    own reproducible seed derived deterministically from a single master
##    seed (so the whole study reruns identically end to end).
## ---------------------------------------------------------------------------
master_seed <- 20260908                       # change once, per study
set.seed(master_seed)

sim_conditions <- expand.grid(
  N   = c(500, 1000, 2000),                   # sample-size conditions
  rep = 1:100,                                # replications per condition
  KEEP.OUT.ATTRS = FALSE
)
sim_conditions$seed <- sample(1e5:1e7, nrow(sim_conditions))  # unique reproducible seeds

sim_data_list <- vector("list", nrow(sim_conditions))
for (i in seq_len(nrow(sim_conditions))) {
  sim_data_list[[i]] <- simulate_one(
    N       = sim_conditions$N[i],
    seed    = sim_conditions$seed[i],
    Qc      = Qc,
    gs      = gs_true,
    no.bugs = no.bugs
  )
}
names(sim_data_list) <- paste0("N", sim_conditions$N, "_rep", sim_conditions$rep)

## ---------------------------------------------------------------------------
## 6. Save everything needed to reproduce and later score the study
## ---------------------------------------------------------------------------
saveRDS(
  list(
    master_seed    = master_seed,
    Qc             = Qc,
    no.bugs        = no.bugs,
    gs_true        = gs_true,
    conditions     = sim_conditions,
    data           = sim_data_list
  ),
  file = "sequential_SISM_simulation_data.rds"
)

cat(sprintf(
  "Generated %d replications across N = %s (master seed = %d).\n",
  nrow(sim_conditions),
  paste(unique(sim_conditions$N), collapse = ", "),
  master_seed
))

## ---------------------------------------------------------------------------
## Usage in the estimation loop (see SISM_polytomous_GDINA.R):
##
##   sim_study <- readRDS("sequential_SISM_simulation_data.rds")
##   cond      <- sim_study$data[["N1000_rep1"]]
##   dat       <- cond$dat
##   mod       <- GDINA(dat, sim_study$Qc, model = "SISM",
##                       sequential = TRUE, no.bugs = sim_study$no.bugs,
##                       mono.constraint = TRUE)
##   # compare recovered attributes/parameters against cond$attribute /
##   # cond$catprob.parm / cond$delta.parm for the recovery study
## ---------------------------------------------------------------------------
























#=============================================================================
# Sequential SISM (Skills + Misconceptions) model for POLYTOMOUS responses
# GDINA package (Ma & de la Torre, 2020, JSS)
# =============================================================================
#
# Background
# ----------
# * SISM (Kuo, Chen & de la Torre, 2018) is natively defined for DICHOTOMOUS
#   items. It needs a Q-matrix with skill columns AND bug/misconception
#   columns, fit via GDINA(..., model = "SISM", no.bugs = <k>).
# * Polytomous (ordinal/nominal) responses are handled by the SEQUENTIAL
#   G-DINA extension (Ma & de la Torre, 2016): each polytomous item is split
#   into ordered pseudo-items ("categories"), and any of the CDMs GDINA
#   supports -- including SISM -- can be the processing function at the
#   category level.
# * Combining the two means: sequential = TRUE, model = "SISM", a Qc-matrix
#   (Item + Category + attribute/bug columns), and no.bugs set correctly.
#   Getting any of these wrong silently breaks identifiability, so the
#   checks below are not optional extras -- they're what keeps the
#   estimation sound.
# =============================================================================

library(GDINA)

## ---------------------------------------------------------------------------
## 1. Build the Qc-matrix
## ---------------------------------------------------------------------------
# Format required by GDINA() when sequential = TRUE:
#   col 1        = Item number
#   col 2        = Category number (1, 2, ... up to max score of that item)
#   next K cols  = SKILL attributes (0/1)
#   last B cols  = BUG/misconception attributes (0/1) -- must come last
#
# Example: 5 items measuring 3 skills (A1-A3) and 2 bugs (B1-B2).
# Items 1-2 are polytomous with 3 categories (2 sequential steps each);
# items 3-5 are dichotomous (1 category = 1 step).

Qc <- data.frame(
  Item = c(1,1, 2,2, 3, 4, 5),
  Cat  = c(1,2, 1,2, 1, 1, 1),
  A1   = c(1,1, 0,0, 0, 1, 0),
  A2   = c(0,0, 1,1, 0, 1, 1),
  A3   = c(0,0, 0,0, 1, 0, 1),
  B1   = c(1,0, 1,0, 1, 0, 1),
  B2   = c(0,1, 0,1, 0, 1, 1)
)

no.bugs <- 2                          # number of bug columns (must match layout above)
K.skills <- ncol(Qc) - 2 - no.bugs    # number of real skill attributes

## ---------------------------------------------------------------------------
## 2. Data requirements
## ---------------------------------------------------------------------------
# `dat` must be N x J, one column PER ITEM (not per category), scored
# 0, 1, 2, ... up to that item's max category. GDINA() internally expands
# these into the sequential pseudo-item responses using Item/Cat in Qc.
#
# dat <- read.csv("your_polytomous_data.csv")

## Sanity checks -- run these BEFORE fitting. A mismatch here is the single
## most common cause of silently wrong SISM/sequential estimates.
check_polytomous_data <- function(dat, Qc, min_n_per_cat = 20) {
  items <- unique(Qc$Item)
  ok <- TRUE
  for (j in items) {
    max_cat <- max(Qc$Cat[Qc$Item == j])          # highest valid score for item j
    obs     <- sort(unique(na.omit(dat[[j]])))
    if (!all(obs %in% 0:max_cat)) {
      ok <- FALSE
      warning(sprintf(
        "Item %d: response codes %s fall outside the expected range 0:%d given Qc.",
        j, paste(obs, collapse = ","), max_cat))
    }
    tab <- table(dat[[j]])
    if (any(tab < min_n_per_cat)) {
      warning(sprintf(
        "Item %d: some score categories have fewer than %d respondents -- SISM's
delta_j0/delta_j1/delta_j2/delta_j12 parameters for that step may be unstable.",
        j, min_n_per_cat))
    }
  }
  invisible(ok)
}
check_polytomous_data(dat, Qc)

## ---------------------------------------------------------------------------
## 3. Fit the sequential SISM model
## ---------------------------------------------------------------------------
mod_seqSISM <- GDINA(
  dat             = dat,
  Q               = Qc,
  model           = "SISM",
  sequential      = TRUE,
  no.bugs         = no.bugs,
  mono.constraint = TRUE,                 # enforce P(mastery-consistent step) monotone
  control         = list(nstarts = 5,     # multiple random starts -> avoid local optima
                         conv.crit = 1e-5,
                         conv.type = c("ip","mp"))
)

## ---------------------------------------------------------------------------
## 4. Diagnostics -- confirm the estimation is actually sound
## ---------------------------------------------------------------------------
summary(mod_seqSISM)
extract(mod_seqSISM, "sequential")        # should be TRUE

# 4a. Monotonicity: SISM's ordering (masters skills+no bugs > masters skills only
#     > no bugs only > neither) must hold empirically, not just by assumption.
monocheck(mod_seqSISM)

# 4b. Convergence / boundary values: probabilities pinned at the lower/upper
#     bound (control$lower.p / upper.p, default .0001/.9999) signal a
#     category that's too sparse to identify SISM's 4 parameters.
delta_est <- coef(mod_seqSISM, what = "delta")
print(delta_est)

# 4c. Parameter count vs. sample size -- rough identifiability check.
n_par  <- npar(mod_seqSISM)
n_obs  <- nrow(dat)
cat(sprintf("Parameters: %d | N: %d | obs-per-parameter: %.1f\n",
            n_par$`np.total`, n_obs, n_obs / n_par$`np.total`))
# Rule of thumb: aim for at least ~20-30 observations per estimated
# parameter; below that, consider att.dist = "higher.order" to cut down
# the joint-attribute-distribution parameters, or simplify some
# categories from SISM to a reduced model (e.g. "DINA") if bugs are
# implausible for that step.

# 4d. Model + item fit
modelfit(mod_seqSISM)
itemfit(mod_seqSISM)

# 4e. Compare full SISM against a reduced alternative to confirm the
#     skill/bug interaction structure is actually needed (not just assumed).
mod_seqBUGDINO <- GDINA(dat, Qc, model = "BUGDINO", sequential = TRUE, no.bugs = no.bugs)
anova(mod_seqSISM, mod_seqBUGDINO)