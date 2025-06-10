library(rstan)
library(ggplot2)
library(reshape2) 
library(loo)
library(dplyr)
library(gridExtra)
library(readxl)

#######################################################################################################

# DISC data 
n_p <- 901     # number of people
n_i <- 40      # number of items 
n_r <- 5       # number of response options
n_t <- n_r - 1 # number of thresholds
n_d <- 4       # number of dimensions
n_c <- 2^n_d   # number of profiles  
n_id <- 10     # number of items per dimension 

#Descriptive stats
disc <- read_excel("discdat.xlsx")  

path <- file.path(getwd(), 'Item distributions/')
dir.create(path)

for (i in 1:ncol(disc)) {
  p <- ggplot(disc, aes(x = factor(disc[[i]]))) +
    geom_bar() +
    labs(title = paste("Item", i), x = "Agreement Level", y = "Frequency") +
    theme_minimal()
  filename <- paste("item_", i, ".jpg", sep = "") 
  filepath = file.path(path, filename) 
  ggsave(filepath, plot = p, width = 7, height = 6, dpi = 500, units = "in", device='jpg')
}

# Stanfit object diagnostics 

rrdm@model_pars
nrdm@model_pars
rsdm@model_pars

rsdm@date #version control

check_divergences(rsdm)

# LOOIC

loglik1 <- extract(rrdm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff1 <- relative_eff(exp(loglik1)) 
loo1 <- loo(loglik1, r_eff = r_eff1)
print(loo1)


loglik2 <- extract(nrdm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff2 <- relative_eff(exp(loglik2)) 
loo2 <- loo(loglik2, r_eff = r_eff2)
print(loo2)


loglik3 <- extract(rsdm, "contributionsI", permuted = F, inc_warmup = F,include = T)
r_eff3 <- relative_eff(exp(loglik3)) 
loo3 <- loo(loglik3, r_eff = r_eff3)
print(loo3)


###############################################################################################################
# Item Plots 
rrdm_table  <- summary(rrdm)$summary
nrdm_table  <- summary(nrdm)$summary
rsdm_table  <- summary(rsdm)$summary


#RSDM Plots
# extracted parameters for RSDM 

start_idx <- seq(from = n_c + 1, by = n_i, length.out = 2)
end_idx <- start_idx + n_i - 1

item_r <- data.frame(
  mapply(function(start, end) rsdm_table[start:end, 1], start_idx, end_idx)
)

start_idx <- seq(from = n_c + 2*n_i + 1, by = n_d, length.out = 2*n_t)
end_idx <- start_idx + n_d - 1

step_r <- data.frame(
  mapply(function(start, end) rsdm_table[start:end, 1], start_idx, end_idx)
)
step_r <- step_r[rep(seq_len(nrow(step_r)), each = n_id), ]

colnames(item_r) <- c("i_I","i_M")
rownames(item_r) <- paste0("item_", seq(1, n_i))

colnames(step_r) <- paste0(rep(c("i", "m"), each = n_t), "_step", rep(1:n_t, 2))
rownames(step_r) <- paste("item", rep(1:n_i, 1), "_D", rep(1:n_d, each = n_id), sep="")


t <- matrix(NA,n_i,n_r) 
k <- 1 
for(i in 1:n_i) {
  t2 <- exp((item_r[i,2]+step_r[i,5])*k+item_r[i,1]-step_r[i,1])
  t3 <- exp((item_r[i,2]+step_r[i,5]+step_r[i,6])*k+item_r[i,1]-step_r[i,1]-step_r[i,2])
  t4 <- exp((item_r[i,2]+step_r[i,5]+step_r[i,6]+step_r[i,7])*k+item_r[i,1]-step_r[i,1]-step_r[i,2]-step_r[i,3])
  t5 <- exp((item_r[i,2]+step_r[i,5]+step_r[i,6]+step_r[i,7]+step_r[i,8])*k+item_r[i,1]-step_r[i,1]-step_r[i,2]-step_r[i,3]-step_r[i,4])
  sum <- 1 + t2 + t3 + t4 +t5
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum, t5/sum)
}
t1 <- t 
t <- matrix(NA,n_i,n_r) 
k <- 0 
t0 <- t 
model.t <- rbind(t0,t1)

# Model Item Plots
model.t <- round(model.t,4)
gr <- c(rep("No attribute",n_i),rep("Attribute",n_i),rep("o0",n_i),rep("o1",n_i))
id <- c(rep(c(1:n_i),4))
model.t <- data.frame(cbind(id,gr,model.t))
colnames(model.t) <- c("item","class", "Strongly Disagree", 
                       "Disagree", "Neutral", "Agree", "Strongly Agree")
sapply(model.t, class)
model.t[,3:7] <- sapply(model.t[,3:7],as.numeric)
summary(model.t)

# choose from dfm_r, dfm_n, or dfm_rs
dfm_rs <- melt(model.t, id.vars=c("item", "class"),
              measure.vars = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))

# save the plots for every item

path <- file.path(getwd(), 'RSDM plots/')
dir.create(path)
for (i in 1:n_i){
  item <- subset(dfm_rs,item==i)
  p <- ggplot(item, aes(x=variable,y=value,group=class)) +
    geom_line(aes(color=class))+
    geom_point(aes(color=class))+
    theme_light()+
    ggtitle(paste("FDCM: probability to select a response option on item", i))+ 
    xlab("Response options")+
    ylab("Probability")
  #p
  filename <- paste("item_", i, ".png", sep = "") 
  filepath = file.path(path, filename) 
  ggsave(filepath, plot = p, width = 7, height = 6, dpi = 500, units = "in", device='png')
}
