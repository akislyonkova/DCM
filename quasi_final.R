library(GDINA)
library(MatchIt)
#library(mvtnorm)

run_simulation <- function(N, gs_param, n_reps = 500) {
  results <- data.frame(bias = numeric(n_reps), tau_est = numeric(n_reps))
  
  for (i in 1:n_reps) {
    set.seed(i)
    K <- 3
    J <- 15
    tau_true <- 0.5
    
    Q <- matrix(0, J, K)
    Q[1:3, 1] <- 1
    Q[4:6, 2] <- 1
    Q[7:9, 3] <- 1
    Q[10:11, c(1,2)] <- 1
    Q[12:13, c(2,3)] <- 1
    Q[14:15, c(1,3)] <- 1
    
    sim_data <- simGDINA(N, Q, gs.parm = matrix(gs_param, J, 2), model = "GDINA")
    item_responses <- sim_data$dat
    alpha_true <- sim_data$attribute
    
    beta_0 <- -2.5  
    beta_1 <- 0.8; beta_2 <- 0.8; beta_3 <- 0.8
    logit_pz <- beta_0 + beta_1*alpha_true[,1] + beta_2*alpha_true[,2] + beta_3*alpha_true[,3]
    prob_z <- plogis(logit_pz)
    Z <- rbinom(N, size = 1, prob = prob_z)
    
    gamma_0 <- 0
    gamma_1 <- 1.0; gamma_2 <- 1.0; gamma_3 <- 1.0
    epsilon <- rnorm(N, mean = 0, sd = 1)
    Y <- gamma_0 + tau_true*Z + gamma_1*alpha_true[,1] + gamma_2*alpha_true[,2] + gamma_3*alpha_true[,3] + epsilon
    
    model_fit <- GDINA(dat = item_responses, Q = Q, model = "GDINA", verbose = 0)
    alpha_est <- personparm(model_fit, what = "MAP")
    
    df_est <- data.frame(Y = Y, Z = Z,
                         A1_est = alpha_est[,1],
                         A2_est = alpha_est[,2],
                         A3_est = alpha_est[,3])
    
    match_out <- matchit(Z ~ A1_est + A2_est + A3_est,
                         data = df_est, 
                         method = "full", # every stratum contains at least 1 treated unit and at least 1 control unit
                         distance = "glm", # logistic because treatment is binary 
                         discard = "both") # drop units that fall outside the area of common support
    matched_data <- match.data(match_out)
    
    bal <- summary(match_out, un = FALSE)$sum.matched
    mean_smd <- mean(abs(bal[, "Std. Mean Diff."]))
    results$mean_smd[i] <- mean_smd
    
    att_model <- lm(Y ~ Z + A1_est + A2_est + A3_est, data = matched_data, weights = weights)
    tau_est <- coef(att_model)["Z"]
    bias <- tau_est - tau_true
    
    results$bias[i] <- bias
    results$tau_est[i] <- tau_est
  }
  
  return(list(
    results    = results,
    mean_bias  = mean(results$bias),
    rmse       = sqrt(mean(results$bias^2)),
    mean_smd   = mean(results$mean_smd)       # average balance across reps
  ))
}

conditions <- list(
  list(N = 500,  gs = 0.1),
  list(N = 1000, gs = 0.1),
  list(N = 500,  gs = 0.3),
  list(N = 1000, gs = 0.3)
)

# Run all conditions and save to a named list in your environment
simulation_results <- lapply(conditions, function(cond) {
  run_simulation(N = cond$N, gs_param = cond$gs)
})

# Name each element for easy access
names(simulation_results) <- c("N500_gs10", "N1000_gs10", "N500_gs30", "N1000_gs30")



library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)   # install.packages("patchwork") if needed

# ── 0. Load results ────────────────────────────────────────────────────────────
simulation_results <- readRDS("simulation_results.rds")   # adjust path if needed

# ── 1. Build a tidy summary data frame ────────────────────────────────────────
conditions <- data.frame(
  condition = names(simulation_results),
  N         = c(500, 1000, 500, 1000),
  gs        = c(0.10, 0.10, 0.30, 0.30)
)

tau_true <- 0.5

summary_df <- conditions |>
  mutate(
    mean_bias  = sapply(simulation_results, function(x) x$mean_bias),
    rmse       = sapply(simulation_results, function(x) x$rmse),
    mean_tau   = sapply(simulation_results, function(x) mean(x$results$tau_est)),
    sd_tau     = sapply(simulation_results, function(x) sd(x$results$tau_est)),
    mean_smd   = sapply(simulation_results, function(x) x$mean_smd),
    label      = paste0("N=", N, "\ngs=", gs)
  )

# ── 2. Build a tidy per-rep data frame (for density / boxplot) ────────────────
reps_df <- mapply(function(res, nm, n, gs) {
  res$results |>
    mutate(condition = nm, N = n, gs = gs,
           label = paste0("N=", n, "\ngs=", gs))
},
res = simulation_results,
nm  = names(simulation_results),
n   = conditions$N,
gs  = conditions$gs,
SIMPLIFY = FALSE
) |> bind_rows()

# ── 3. Shared theme & colours ─────────────────────────────────────────────────
cond_colors <- c(
  "N500_gs10"  = "#378ADD",
  "N1000_gs10" = "#1D9E75",
  "N500_gs30"  = "#D85A30",
  "N1000_gs30" = "#7F77DD"
)

base_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(colour = "grey40"),
    axis.title         = element_text(colour = "grey30", size = 11),
    strip.text         = element_text(face = "bold"),
    legend.position    = "none"
  )

# ── 4. Plot 1 — Mean bias bar chart ───────────────────────────────────────────
p_bias <- ggplot(summary_df, aes(x = label, y = mean_bias, fill = condition)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.5) +
  geom_text(aes(label = round(mean_bias, 3),
                vjust = ifelse(mean_bias >= 0, -0.5, 1.3)),
            size = 3.5, colour = "grey30") +
  scale_fill_manual(values = cond_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  labs(x = NULL, y = expression(hat(tau) - tau),
       title = "Mean bias by condition") +
  base_theme

# ── 5. Plot 2 — RMSE bar chart ────────────────────────────────────────────────
p_rmse <- ggplot(summary_df, aes(x = label, y = rmse, fill = condition)) +
  geom_col(width = 0.6, alpha = 0.6) +
  geom_text(aes(label = round(rmse, 3)), vjust = -0.5,
            size = 3.5, colour = "grey30") +
  scale_fill_manual(values = cond_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "RMSE", title = "RMSE by condition") +
  base_theme

# ── 6. Plot 3 — Bias distribution (density) ───────────────────────────────────
p_density <- ggplot(reps_df, aes(x = bias, fill = condition, colour = condition)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  scale_fill_manual(values = cond_colors) +
  scale_colour_manual(values = cond_colors) +
  facet_wrap(~ label, nrow = 1) +
  labs(x = expression(hat(tau) - tau), y = "Density",
       title = "Bias distribution across replications") +
  base_theme +
  theme(panel.grid.major.x = element_line(colour = "grey92"))

# ── 7. Plot 4 — τ̂ distribution (violin + boxplot) ────────────────────────────
p_tau <- ggplot(reps_df, aes(x = label, y = tau_est, fill = condition, colour = condition)) +
  geom_violin(alpha = 0.25, linewidth = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.15, alpha = 0.6, outlier.size = 0.8, outlier.alpha = 0.4,
               colour = "grey30") +
  geom_hline(yintercept = tau_true, linetype = "dashed",
             colour = "grey30", linewidth = 0.6) +
  annotate("text", x = 4.6, y = tau_true, label = paste0("τ = ", tau_true),
           hjust = 1, vjust = -0.5, size = 3.2, colour = "grey30") +
  scale_fill_manual(values = cond_colors) +
  scale_colour_manual(values = cond_colors) +
  labs(x = NULL, y = expression(hat(tau)),
       title = expression("Distribution of " * hat(tau) * " across replications")) +
  base_theme

# ── 8. Plot 5 — Mean SMD bar chart ────────────────────────────────────────────
p_smd_bar <- ggplot(summary_df, aes(x = label, y = mean_smd, fill = condition)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_hline(yintercept = 0.1, linetype = "dashed", colour = "tomato3", linewidth = 0.5) +
  annotate("text", x = 4.6, y = 0.1, label = "SMD = 0.10",
           hjust = 1, vjust = -0.5, size = 3.2, colour = "tomato3") +
  geom_text(aes(label = round(mean_smd, 3)), vjust = -0.5,
            size = 3.5, colour = "grey30") +
  scale_fill_manual(values = cond_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(x = NULL, y = "Mean |SMD|",
       title = "Post-matching balance: mean |SMD| by condition") +
  base_theme

# ── 9. Plot 6 — SMD distribution (density) ────────────────────────────────────
p_smd_dens <- ggplot(reps_df, aes(x = mean_smd, fill = condition, colour = condition)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  geom_vline(xintercept = 0.1, linetype = "dashed",
             colour = "tomato3", linewidth = 0.5) +
  scale_fill_manual(values = cond_colors) +
  scale_colour_manual(values = cond_colors) +
  facet_wrap(~ label, nrow = 1) +
  labs(x = "Mean |SMD|", y = "Density",
       title = "SMD distribution across replications") +
  base_theme +
  theme(panel.grid.major.x = element_line(colour = "grey92"))

# ── 10. Assemble & save — Panel 1: bias/RMSE (original) ──────────────────────
panel_bias_rmse <- (p_bias | p_rmse) / p_density +
  plot_annotation(
    title    = "Simulation results: bias and RMSE",
    subtitle = "500 replications per condition | true τ = 0.5",
    theme    = theme(
      plot.title    = element_text(size = 14, face = "bold", colour = "grey20"),
      plot.subtitle = element_text(size = 11, colour = "grey40")
    )
  )

ggsave("simulation_bias_plots.png", panel_bias_rmse,
       width = 12, height = 8, dpi = 300, bg = "white")
message("Saved: simulation_bias_plots.png")

# ── 11. Panel 2: τ̂ estimates ─────────────────────────────────────────────────
panel_tau <- p_tau +
  plot_annotation(
    title    = expression("Estimated treatment effect " * hat(tau)),
    subtitle = "500 replications per condition | dashed line = true τ = 0.5",
    theme    = theme(
      plot.title    = element_text(size = 14, face = "bold", colour = "grey20"),
      plot.subtitle = element_text(size = 11, colour = "grey40")
    )
  )

ggsave("simulation_tau_plots.png", panel_tau,
       width = 10, height = 5, dpi = 300, bg = "white")
message("Saved: simulation_tau_plots.png")

# ── 12. Panel 3: SMD balance ──────────────────────────────────────────────────
panel_smd <- p_smd_bar / p_smd_dens +
  plot_annotation(
    title    = "Post-matching covariate balance (standardised mean difference)",
    subtitle = "500 replications per condition | dashed line = SMD threshold 0.10",
    theme    = theme(
      plot.title    = element_text(size = 14, face = "bold", colour = "grey20"),
      plot.subtitle = element_text(size = 11, colour = "grey40")
    )
  )

ggsave("simulation_smd_plots.png", panel_smd,
       width = 12, height = 8, dpi = 300, bg = "white")
message("Saved: simulation_smd_plots.png")

