library(GDINA)
set.seed(2026)


K.skills <- 3         # A1, A2, A3  (skill attributes)
no.bugs  <- 2         # B1, B2      (misconception/bug attributes)
K        <- K.skills + no.bugs
att_cols <- c("A1", "A2", "A3", "B1", "B2")


Qc <- data.frame(
  Item = c(1, 1, 2, 2, 3, 4, 5),
  Cat  = c(1, 2, 1, 2, 1, 1, 1),
  A1   = c(1, 1, 0, 0, 0, 1, 0),
  A2   = c(0, 0, 1, 1, 0, 1, 1),
  A3   = c(0, 0, 0, 0, 1, 0, 1),
  B1   = c(1, 0, 1, 0, 1, 0, 1),
  B2   = c(0, 1, 0, 1, 0, 1, 1)
)

n_cats  <- nrow(Qc)                    # 7 pseudo-categories total
J_items <- length(unique(Qc$Item))     # 5 real items

print(Qc)
N <- 2000   

true_att <- matrix(rbinom(N * K, 1, 0.5), nrow = N, ncol = K,
                   dimnames = list(NULL, att_cols))


p00 <- 0.10; p10 <- 0.35; p01 <- 0.35; p11 <- 0.85

delta <- data.frame(
  d0  = rep(p00, n_cats),
  d1  = rep(p10 - p00, n_cats),
  d2  = rep(p01 - p00, n_cats),
  d12 = rep(p11 - p10 - p01 + p00, n_cats)
)
rownames(delta) <- paste0("Item", Qc$Item, "_Cat", Qc$Cat)
stopifnot(all(rowSums(delta[, c("d0","d1","d2","d12")]) <= 1))
print(delta)

sim_step <- function(r, true_att) {
  qrow      <- as.numeric(Qc[r, att_cols])
  skill_pos <- which(qrow[1:K.skills] == 1)
  bug_pos   <- K.skills + which(qrow[(K.skills + 1):K] == 1)
  
  master  <- apply(true_att[, skill_pos, drop = FALSE] == 1, 1, all)
  bugfree <- apply(true_att[, bug_pos,   drop = FALSE] == 0, 1, all)
  
  p <- delta$d0[r] + delta$d1[r] * master + delta$d2[r] * bugfree +
    delta$d12[r] * (master & bugfree)
  
  rbinom(nrow(true_att), 1, p)
}

step_mat <- sapply(seq_len(n_cats), sim_step, true_att = true_att)
colnames(step_mat) <- rownames(delta)


dat <- matrix(NA_integer_, N, J_items)
colnames(dat) <- paste0("Item", seq_len(J_items))

for (j in seq_len(J_items)) {
  cols  <- which(Qc$Item == j)
  cols  <- cols[order(Qc$Cat[cols])]
  score   <- rep(0L, N)
  reached <- rep(TRUE, N)
  for (cc in cols) {
    score[reached]   <- score[reached] + step_mat[reached, cc]
    reached           <- reached & (step_mat[, cc] == 1)
  }
  dat[, j] <- score
}
dat <- as.data.frame(dat)

cat("\nPolytomous score distributions:\n")
print(lapply(dat, table))

custom_sism <- function(Qrow, K.skills, no.bugs) {
  qrow      <- as.numeric(Qrow)
  skill_pos <- which(qrow[1:K.skills] == 1)
  bug_pos   <- K.skills + which(qrow[(K.skills + 1):(K.skills + no.bugs)] == 1)
  req_pos   <- sort(c(skill_pos, bug_pos))
  is_skill  <- req_pos %in% skill_pos
  Kj        <- length(req_pos)
  
  patt    <- attributepattern(Kj)              
  master  <- apply(patt[, is_skill,  drop = FALSE] == 1, 1, all)
  bugfree <- apply(patt[, !is_skill, drop = FALSE] == 0, 1, all)
  
  cbind(intercept = 1,
        skill     = as.numeric(master),
        bug       = as.numeric(bugfree),
        inter     = as.numeric(master & bugfree))
}

D_list <- lapply(seq_len(n_cats), function(r) {
  custom_sism(Qc[r, att_cols], K.skills, no.bugs)
})
names(D_list) <- rownames(delta)


fit <- GDINA(dat = dat, Q = Qc,
             model         = rep("UDF", n_cats),
             sequential    = TRUE,
             design.matrix = D_list,
             linkfunc      = rep("identity", n_cats),
             control       = list(nstarts = 5))
