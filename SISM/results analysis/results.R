# =============================================================================
# Simulation Study 1 – Parameter Bias & Profile Recovery Analysis
# =============================================================================


library(ggplot2)
library(dplyr)
library(tidyr)


load("Study1_data.RData")          
load("study1_results.Rdata")       


compute_condition <- function(cond_idx) {
  
  cond   <- data[[cond_idx]]$condition
  reps   <- data[[cond_idx]]$replications
  res    <- final_results[[cond_idx]]
  n_reps <- length(reps)
  n_items <- length(reps[[1]]$itemprob)
  
  
  all_diffs <- unlist(lapply(seq_len(n_reps), function(r) {
    unlist(lapply(seq_len(n_items), function(j) {
      true_p <- reps[[r]]$itemprob[[j]]
      est_p  <- res[[r]]$estimates[[j]]
      common <- intersect(names(true_p), names(est_p))
      if (length(common) == 0) return(NULL)
      est_p[common] - true_p[common]
    }))
  }))
  
  mean_bias     <- mean(all_diffs, na.rm = TRUE)
  mean_abs_bias <- mean(abs(all_diffs), na.rm = TRUE)
  rmse          <- sqrt(mean(all_diffs^2, na.rm = TRUE))
  
 
  pcr <- mean(vapply(seq_len(n_reps), function(r) {
    tp <- reps[[r]]$profiles
    ep <- res[[r]]$profiles
    mean(apply(tp == ep, 1, all))
  }, numeric(1)))
  
  
  K <- ncol(reps[[1]]$profiles)
  attr_acc <- colMeans(do.call(rbind, lapply(seq_len(n_reps), function(r) {
    tp <- reps[[r]]$profiles
    ep <- res[[r]]$profiles
    colMeans(tp == ep)
  })))
  names(attr_acc) <- paste0("attr", seq_len(K))
  
  
  base <- data.frame(
    N             = as.integer(trimws(cond["N"])),
    J             = as.integer(trimws(cond["J"])),
    qual          = cond["qual_name"],
    a_type        = cond["a_type"],
    e_type        = cond["e_type"],
    e_rate        = as.numeric(cond["e_rate"]),
    mean_bias     = round(mean_bias,     4),
    mean_abs_bias = round(mean_abs_bias, 4),
    rmse          = round(rmse,          4),
    profile_recovery = round(pcr,        4),
    row.names     = NULL,
    stringsAsFactors = FALSE
  )
  attr_df <- as.data.frame(t(round(attr_acc, 4)))
  cbind(base, attr_df)
}


cat("Running analysis for all 64 conditions...\n")
summary_df <- do.call(rbind, lapply(seq_along(data), compute_condition))
cat("Done.\n\n")

print(summary_df[, c("N","J","qual","a_type","e_type","e_rate",
                     "mean_bias","mean_abs_bias","rmse","profile_recovery")])

write.csv(summary_df, "simulation_summary.csv", row.names = FALSE)


# ── Plots ──────────────────────────────────────────────────────────────────
theme_set(theme_bw(base_size = 12))

plot_df <- summary_df %>%
  mutate(
    Condition = paste0("N=", N, ", J=", J),
    Error     = paste0(e_type, " (", e_rate, ")")
  )


p_bias <- ggplot(plot_df,
                 aes(x = Condition, y = interaction(a_type, qual, sep = "\n"),
                     fill = mean_abs_bias)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.3f", mean_abs_bias)), size = 2.8) +
  scale_fill_gradient(low = "#d4f1c4", high = "#c0392b",
                      name = "MAB") +
  facet_grid(Error ~ ., switch = "y") +
  labs(title = "Mean Absolute Bias of Item Response Probability Estimates",
       x = "Sample Size × Items", y = "Attribute type × Q-quality") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text.y.left = element_text(angle = 0))

ggsave("bias_heatmap.png", p_bias, width = 11, height = 8, dpi = 150)


p_pcr <- ggplot(plot_df,
                aes(x = Condition, y = profile_recovery,
                    fill = qual, colour = a_type)) +
  geom_col(position = position_dodge(0.85), width = 0.75,
           linewidth = 0.5) +
  scale_fill_manual(values = c(High = "#2ecc71", Low = "#e74c3c"),
                    name = "Q-quality") +
  scale_colour_manual(values = c(Skill = "#2c3e50", Misconception = "#8e44ad"),
                      name = "Attribute type") +
  facet_grid(e_rate ~ e_type, labeller = label_both) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Correct Profile Recovery Rate",
       x = "N × J", y = "Proportion correctly classified") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave("profile_recovery_plot.png", p_pcr, width = 12, height = 7, dpi = 150)


attr_cols <- grep("^attr", names(summary_df), value = TRUE)

attr_long <- summary_df %>%
  select(N, J, qual, a_type, e_type, e_rate, all_of(attr_cols)) %>%
  pivot_longer(cols = all_of(attr_cols),
               names_to  = "Attribute",
               values_to = "Accuracy") %>%
  mutate(
    Condition = paste0("N=", N, ", J=", J),
    Group     = paste0(qual, " / ", a_type)
  )

p_attr <- ggplot(attr_long,
                 aes(x = Attribute, y = Accuracy,
                     colour = Group, group = Group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_colour_brewer(palette = "Dark2", name = "Q-quality / Attr. type") +
  facet_grid(interaction(e_type, e_rate, sep = " ") ~ Condition,
             labeller = label_value) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Attribute-Level Classification Accuracy",
       x = "Attribute", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("attr_recovery_plot.png", p_attr, width = 14, height = 9, dpi = 150)