custom_sism <- function(Qc, K.skills, no.bugs) {
  design.matrix <- vector("list", nrow(Qc))
  for (r in seq_len(nrow(Qc))) {
    qrow      <- as.numeric(Qc[r, -(1:2)])
    skill_pos <- which(qrow[1:K.skills] == 1)
    bug_pos   <- K.skills + which(qrow[(K.skills + 1):(K.skills + no.bugs)] == 1)
    req_pos   <- sort(c(skill_pos, bug_pos))
    is_skill  <- req_pos %in% skill_pos
    Kj        <- length(req_pos)
    
    patt <- attributepattern(Kj)                       # same enumeration GDINA uses internally
    skill_mastered <- apply(patt[, is_skill,  drop = FALSE], 1, function(b) as.numeric(all(b == 1)))
    bug_free       <- apply(patt[, !is_skill, drop = FALSE], 1, function(b) as.numeric(all(b == 0)))
    
    design.matrix[[r]] <- cbind(intercept = 1,
                                skill = skill_mastered,
                                bug   = bug_free,
                                inter = skill_mastered * bug_free)
  }
  design.matrix
}

current_data <- demo_data                                              # raw polytomous scores
current_Q    <- demo_qmis
D_list       <- custom_sism(current_Q, K.skills, no.bugs)
n_cats       <- nrow(current_Q)

fit_attempt <- tryCatch({
  GDINA(dat = current_data, Q = current_Q,
        model = rep("UDF", n_cats), sequential = TRUE,
        design.matrix = D_list, linkfunc = rep("identity", n_cats),
        verbose = 0,
        control = list(nstarts = 5, conv.crit = 1e-5, conv.type = c("ip","mp")))
}, error = function(e) { ... })


