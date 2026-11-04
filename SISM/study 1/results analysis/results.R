library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(scales)

load("Study1_data.RData")
load("study1_results.Rdata")

stopifnot(length(data) == length(final_results))

all_fits <- unlist(final_results, recursive = FALSE)
n_success <- sum(map_lgl(all_fits, ~ isTRUE(.x$success)))
cat(sprintf("Successful fits: %d of %d\n", n_success, length(all_fits)))

as_item_by_class <- function(M, n_items, n_classes) {
  M <- unname(as.matrix(M))
  if (nrow(M) == n_items && ncol(M) == n_classes) return(M)
  if (nrow(M) == n_classes && ncol(M) == n_items) return(t(M))
  stop("Unexpected dimensions for class-level probabilities")
}

rep_metrics <- function(true_rep, fit_rep) {
  if (!isTRUE(fit_rep$success) || is.null(fit_rep$LCprob) || is.null(fit_rep$profiles)) {
    return(NULL)
  }

  tp <- unname(as.matrix(true_rep$profiles))
  ep <- unname(as.matrix(fit_rep$profiles))
  stopifnot(identical(dim(tp), dim(ep)))

  n_items   <- nrow(true_rep$Q_true)
  n_classes <- 2^ncol(tp)

  P_true <- as_item_by_class(true_rep$LCprob, n_items, n_classes)
  P_est  <- as_item_by_class(fit_rep$LCprob,  n_items, n_classes)

  d   <- P_est - P_true
  hit <- tp == ep

  c(mean_bias     = mean(d),
    mean_abs_bias = mean(abs(d)),
    rmse          = sqrt(mean(d^2)),
    pcr           = mean(rowSums(hit) == ncol(hit)),
    setNames(colMeans(hit), paste0("attr", seq_len(ncol(hit)))))
}

compute_condition <- function(i) {
  cond <- as.list(data[[i]]$condition)
  reps <- data[[i]]$replications
  res  <- final_results[[i]]

  metrics <- compact(lapply(seq_along(reps), function(r) rep_metrics(reps[[r]], res[[r]])))
  if (length(metrics) == 0) stop(sprintf("No valid replications in condition %d", i))

  mat <- do.call(rbind, metrics)
  avg <- colMeans(mat)

  base <- data.frame(
    condition = i,
    N         = as.integer(trimws(cond$N)),
    J         = as.integer(trimws(cond$J)),
    qual      = as.character(cond$qual_name),
    a_type    = as.character(cond$a_type),
    e_type    = as.character(cond$e_type),
    e_rate    = as.numeric(trimws(cond$e_rate)),
    n_valid   = nrow(mat),
    stringsAsFactors = FALSE
  )

  cbind(base, as.data.frame(t(round(avg, 4)))) %>%
    rename(profile_recovery = pcr)
}

summary_df <- do.call(rbind, lapply(seq_along(data), compute_condition))

print(summary_df[, c("N", "J", "qual", "a_type", "e_type", "e_rate", "n_valid",
                     "mean_bias", "mean_abs_bias", "rmse", "profile_recovery")])

write.csv(summary_df, "simulation_summary.csv", row.names = FALSE)

theme_set(theme_bw(base_size = 12))

plot_df <- summary_df %>%
  mutate(
    Condition = paste0("N=", N, ", I=", J),
    Error     = paste0(e_type, " (", e_rate, ")"),
    QA_group  = interaction(qual, a_type, sep = " / ")
  )

mab_cut  <- mean(range(plot_df$mean_abs_bias))
rmse_cut <- mean(range(plot_df$rmse))

p_bias <- ggplot(plot_df,
                 aes(x = Condition, y = interaction(a_type, qual, sep = "\n"),
                     fill = mean_abs_bias)) +
  geom_tile(colour = "grey80") +
  geom_text(aes(label = sprintf("%.3f", mean_abs_bias),
                colour = mean_abs_bias > mab_cut),
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
                colour = rmse > rmse_cut),
            size = 3.0) +
  scale_fill_gradient(low = "white", high = "black", name = "RMSE") +
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
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
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
    Condition = paste0("N=", N, ", I=", J),
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
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(title = "Attribute-Level Classification Accuracy",
       x = "Attribute", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("attr_recovery_plot.png", p_attr, width = 14, height = 9, dpi = 150)

summary(summary_df$mean_bias)
summary(summary_df$rmse)
hist(summary_df$rmse)

alpha <- 0.05

design_cols <- summary_df[, c("condition", "N", "J", "qual", "a_type", "e_type", "e_rate")]

fit_by_condition <- map(final_results, function(condition) {

  rep_stats <- map_dfr(condition, function(rep) {
    scalar_stats <- keep(rep$fit_stats, ~ is.numeric(.x) && length(.x) == 1)
    as_tibble(scalar_stats)
  })

  if (!"M2.pvalue" %in% names(rep_stats)) {
    stop("M2.pvalue not found in fit statistics: ", paste(names(rep_stats), collapse = ", "))
  }

  rep_stats %>%
    summarise(
      across(-any_of("M2.pvalue"), ~ mean(.x, na.rm = TRUE)),
      M2.reject = mean(M2.pvalue < alpha, na.rm = TRUE)
    )
})

averaged_fit_stats <- bind_cols(design_cols, bind_rows(fit_by_condition))

write.csv(averaged_fit_stats, file = "averaged_fit_stats.csv", row.names = FALSE)

head(averaged_fit_stats)

hist(averaged_fit_stats$M2.reject)
hist(averaged_fit_stats$SRMSR)
hist(averaged_fit_stats$RMSEA2)
