library(GDINA)

n_reps <- 100

rate_base <- "subQ"

Q_20 <- matrix(c(1,0,0,0,0,0,0, 0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0,
                 0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1, 1,0,0,0,1,0,0,
                 0,1,0,0,1,0,0, 0,0,1,0,0,0,1, 0,0,0,1,0,1,0, 1,1,0,0,1,0,0,
                 1,0,1,0,0,0,1, 1,0,0,1,0,0,1, 0,1,1,0,0,0,1, 0,1,0,1,0,1,1,
                 0,0,1,1,0,1,1, 1,0,1,0,1,1,0, 1,1,0,1,1,1,0, 0,1,1,1,1,1,0),
               ncol = 7, byrow = TRUE)

Q_40 <- rbind(Q_20, Q_20)

design_factors <- expand.grid(
  N = c(500, 1000),
  J = c(20, 40),
  qual_name = c("High", "Low"),
  a_type = c("Skill", "Misconception"),
  e_type = c("Omission", "Inclusion"),
  e_rate = c(0.05, 0.15),
  stringsAsFactors = FALSE
)
write.csv(design_factors, "study1_design_factors.csv", row.names = FALSE)

create_misspecified_Q <- function(Q_base, a_type, e_type, e_rate,
                                  rate_base = c("subQ", "eligible")) {
  rate_base <- match.arg(rate_base)
  Q_mis <- Q_base
  target_cols <- if (a_type == "Skill") 1:4 else 5:7

  target_value <- if (e_type == "Omission") 1 else 0
  target_cells <- which(Q_mis[, target_cols, drop = FALSE] == target_value,
                        arr.ind = TRUE)

  if (e_type == "Omission") {
    valid_rows <- which(rowSums(Q_base) > 1)
    target_cells <- target_cells[target_cells[, "row"] %in% valid_rows, , drop = FALSE]
  }

  target_cells[, 2] <- target_cols[target_cells[, 2]]

  n_base <- if (rate_base == "subQ") nrow(Q_base) * length(target_cols) else nrow(target_cells)
  n_flip <- min(ceiling(e_rate * n_base), nrow(target_cells))

  flip_idx <- sample.int(nrow(target_cells), n_flip)
  for (i in flip_idx) {
    Q_mis[target_cells[i, 1], target_cells[i, 2]] <- if (e_type == "Omission") 0 else 1
  }

  stopifnot(all(rowSums(Q_mis) >= 1))
  stopifnot(sum(Q_mis != Q_base) == n_flip)
  stopifnot(all(which(Q_mis != Q_base, arr.ind = TRUE)[, 2] %in% target_cols))

  Q_mis
}

data <- lapply(seq_len(nrow(design_factors)), function(cond) {
  row       <- design_factors[cond, ]
  N         <- row$N
  J_val     <- row$J
  qual_name <- row$qual_name
  a_type    <- row$a_type
  e_type    <- row$e_type
  e_rate    <- row$e_rate

  Q_base <- if (J_val == 20) Q_20 else Q_40

  gs_val <- if (qual_name == "High") 0.10 else 0.25
  gs_parms <- data.frame(guess = rep(gs_val, J_val), slip = rep(gs_val, J_val))

  cat(sprintf("\nCondition %d: N=%d, J=%d, Qual=%s, Attr=%s, Err=%s, Rate=%.2f\n",
              cond, N, J_val, qual_name, a_type, e_type, e_rate))

  condition_reps <- lapply(1:n_reps, function(rep) {

    set.seed(2026 + 1000 * cond + rep)

    if (rep %% 10 == 0) cat(paste0("..", rep))

    Q_mis <- create_misspecified_Q(Q_base, a_type, e_type, e_rate, rate_base)

    sim <- simGDINA(N = N,
                    Q = Q_base,
                    gs.parm = gs_parms,
                    model = "SISM",
                    no.bugs = 3)

    list(dat      = extract(sim, "dat"),
         Q_true   = Q_base,
         Q_used   = Q_mis,
         itemprob = extract(sim, what = "catprob.parm"),
         delta    = extract(sim, what = "delta.parm"),
         profiles = extract(sim, what = "attribute"))
  })

  list(replications = condition_reps, condition = row)
})

save(data, file = "Study1_data.RData")
