library(GDINA)
library(MatchIt)
library(mvtnorm)

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
    
    beta_0 <- -1.2
    beta_1 <- 0.8; beta_2 <- 0.8; beta_3 <- 0.8
    logit_pz <- beta_0 + beta_1*alpha_true[,1] + beta_2*alpha_true[,2] + beta_3*alpha_true[,3]
    prob_z <- plogis(logit_pz)
    Z <- rbinom(N, size = 1, prob = prob_z)
    
    gamma_0 <- 0
    gamma_1 <- 1.0; gamma_2 <- 1.0; gamma_3 <- 1.0
    epsilon <- rnorm(N, mean = 0, sd = 1)
    Y <- gamma_0 + tau_true*Z + gamma_1*alpha_true[,1] + gamma_2*alpha_true[,2] + gamma_3*alpha_true[,3] + epsilon
    
    model_fit <- GDINA(dat = item_responses, Q = Q, model = "GDINA", verbose = 0)
    alpha_est <- personparm(model_fit, what = "EAP")
    
    df_est <- data.frame(Y = Y, Z = Z,
                         A1_est = alpha_est[,1],
                         A2_est = alpha_est[,2],
                         A3_est = alpha_est[,3])
    
    match_out <- matchit(Z ~ A1_est + A2_est + A3_est,
                         data = df_est, method = "nearest", distance = "glm")
    matched_data <- match.data(match_out)
    
    att_model <- lm(Y ~ Z, data = matched_data, weights = weights)
    tau_est <- coef(att_model)["Z"]
    bias <- tau_est - tau_true
    
    results$bias[i] <- bias
    results$tau_est[i] <- tau_est
  }
  
  return(list(
    results    = results,
    mean_bias  = mean(results$bias),
    rmse       = sqrt(mean(results$bias^2))
  ))
}

conditions <- list(
  list(N = 500,  gs = 0.10),
  list(N = 1000, gs = 0.10),
  list(N = 500,  gs = 0.30),
  list(N = 1000, gs = 0.30)
)

# Run all conditions and save to a named list in your environment
simulation_results <- lapply(conditions, function(cond) {
  run_simulation(N = cond$N, gs_param = cond$gs)
})

# Name each element for easy access
names(simulation_results) <- c("N500_gs10", "N1000_gs10", "N500_gs30", "N1000_gs30")
