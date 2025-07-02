library(rstan)
library(ggplot2)
library(reshape2)


cell <- 1        # cell number 
n_sim <- 25      # number of replications 
n_i <- 10        # number of items 
rows_start <- 5  # first parameter 
rows_end <- 53   # last parameter 
n_id <- 5        # number of items per dimension 

file_name <- paste("RRDM_cell", cell, "_param.txt", sep="")
rrdm_param <- read.table(file_name)


sim_param <- NULL
for (i in 1:n_sim) { 
  sim_name <- sprintf("RRDM_sim%i.txt", i)
  sim_param <- cbind(sim_param,read.table(sim_name)[c(rows_start:rows_end),1])
}

rrdm_param_n <- data.frame(rep(rrdm_param, n_sim))
sim_param_dif <- (sim_param - rrdm_param_n)  
for (i in 1:n_sim) {
  colnames(sim_param_dif)[i] <- paste("sim", i, sep='') 
}

bias <- data.frame(rowSums(sim_param_dif)/n_sim)


colnames(bias) <- "raw_bias" 
rownames(bias) <- paste0("item", seq(1, n_i), "_", rep(c("i", "m", "d"), each = n_i))

bias$rmse <- NA 
bias$rmse <- sqrt((rowSums(sim_param_dif))^2/(n_sim-1)) 


#Overview
hist(bias$raw_bias)
hist(bias$rmse) 

#Bias by parameter type (intercepts, main effects and dispersion for each attribute)
start_idx <- seq(from = 1, by = n_id, length.out = 9)
end_idx <- start_idx + n_id - 1

bias.by.group <- data.frame(
  mapply(function(start, end) bias[start:end, 1], start_idx, end_idx)
)
colnames(bias.by.group) <- paste0(rep(c("i", "m", "d"), each = 3), "_A", rep(1:3, 2))

file_name <- paste("bias summary, cell", cell, ".txt", sep="")
sink(file_name)
summary(bias.by.group[,])
sink()

rmse.by.group <- data.frame(
  mapply(function(start, end) bias[start:end, 2], start_idx, end_idx)
)
colnames(rmse.by.group) <- paste0(rep(c("i", "m", "d"), each = 3), "_A", rep(1:3, 2))

file_name <- paste("rmse summary, cell", cell, ".txt", sep="")
sink(file_name)
summary(rmse.by.group[,])
sink()

bias.by.group_long <- melt(bias.by.group)

title <- paste("Condition ", cell,  sep="")
p <- ggplot(bias.by.group_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  ggtitle(title) +
  geom_hline(yintercept = -0.05, linetype = "dashed", color = "red") +  
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +   
  labs(x = "Parameters by groups", y = "Bias", fill = "Groups") +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
p
title_p <- paste("cell", cell,  "_bias.png", sep="")
ggsave(title_p, 
       plot = p, 
       width = 10, 
       height = 9, 
       dpi = 500, 
       units = "in", 
       device='png', 
       bg = "white")




# Model diagnostics
rrdm@model_pars
check_divergences(rrdm)


l0 <- paste0("l", rep(1:n_i), "I")
l1 <- paste0("l", rep(1:n_i), "M")
step <- c("step1_ID1", "step1_ID2", 
             "step2_ID1", "step2_ID2", 
             "step3_ID1", "step3_ID2", 
             "step4_ID1", "step4_ID2")
rrdm_param <- c("Vc", l0, l1, step)

for (p in rrdm_param){
  print(traceplot(rrdm, pars = p))
}
