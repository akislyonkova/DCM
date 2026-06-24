# =============================================================================
# Simulation Study 1 – Parameter Bias & Profile Recovery Analysis
# =============================================================================


library(ggplot2)
library(dplyr)
library(tidyr)


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
    Error     = paste0(e_type, " (", e_rate, ")"),
    QA_group  = interaction(qual, a_type, sep = " / ")
  )


p_bias <- ggplot(plot_df,
                 aes(x = Condition, y = interaction(a_type, qual, sep = "\n"),
                     fill = mean_abs_bias)) +
  geom_tile(colour = "grey80") +
  geom_text(aes(label = sprintf("%.3f", mean_abs_bias),
                colour = mean_abs_bias > 0.04),    
            size = 3.0) +                          
  scale_fill_gradient(low = "white", high = "black", name = "MAB") +
  scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white"),
                      guide = "none") +             
  facet_grid(Error ~ ., switch = "y") +
  labs(title = "Mean Absolute Bias of Item-Level Estimates",
       x = "Sample Size × Items", y = "Attribute type × Item-quality") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text.y.left = element_text(angle = 0))

ggsave("bias_heatmap.png", p_bias, width = 11, height = 8, dpi = 150)

p_rmse <- ggplot(plot_df,
                 aes(x = Condition, y = interaction(a_type, qual, sep = "\n"),
                     fill = rmse)) +
  geom_tile(colour = "grey80") +
  geom_text(aes(label = sprintf("%.3f", rmse),
                colour = rmse > 0.04),    
            size = 3.0) +                          
  scale_fill_gradient(low = "white", high = "black", name = "MAB") +
  scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white"),
                      guide = "none") +             
  facet_grid(Error ~ ., switch = "y") +
  labs(title = "Root Mean Square Error of Item-Level Estimates",
       x = "Sample Size × Items", y = "Attribute type × Item-quality") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text.y.left = element_text(angle = 0))

ggsave("rmse_heatmap.png", p_rmse, width = 11, height = 8, dpi = 150)

p_pcr <- ggplot(plot_df,
                aes(x = Condition, y = profile_recovery, fill = QA_group)) +
  geom_col(position = position_dodge(0.85), width = 0.75,
           colour = "black", linewidth = 0.6) +          
  scale_fill_manual(
    values = c("white", "grey65", "grey35", "black"),   
    name   = "Item-quality / Attr. type"
  ) +
  facet_grid(e_rate ~ e_type, labeller = label_both) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Pattern Classification Rate",
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
                     colour = Group, linetype = Group,
                     shape = Group, group = Group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_colour_grey(start = 0.0, end = 0.6, name = "Q-quality / Attr. type") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"),
                        name = "Q-quality / Attr. type") +
  scale_shape_manual(values = c(16, 17, 15, 18),
                     name = "Q-quality / Attr. type") +
  facet_grid(interaction(e_type, e_rate, sep = " ") ~ Condition,
             labeller = label_value) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Attribute-Level Classification Accuracy",
       x = "Attribute", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("attr_recovery_plot.png", p_attr, width = 14, height = 9, dpi = 150)

###################################################################################

# Additional statistics 

summary(summary_df$mean_bias)
summary(summary_df$rmse)
hist(summary_df$rmse)


# Model fit 

library(purrr)
library(dplyr)


averaged_fit_stats <- map_dfr(final_results, function(condition) {
  
  rep_stats <- map_dfr(condition, function(rep) {
    
    stats <- rep$fit_stats
    
  
    scalar_stats <- keep(stats, ~ is.numeric(.x) && length(.x) == 1)
    
    as_tibble(scalar_stats)
  })
  
  rep_stats %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
  
}, .id = "condition_id") 

write.csv(averaged_fit_stats, 
          file = "averaged_fit_stats.csv", 
          row.names = FALSE)

head(averaged_fit_stats)

hist(averaged_fit_stats$M2.pvalue)
hist(averaged_fit_stats$SRMSR)
hist(averaged_fit_stats$RMSEA2)
