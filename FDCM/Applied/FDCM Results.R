library(rstan)
library(ggplot2)
library(reshape2) 
library(loo)
library(dplyr)
library(gridExtra)

#######################################################################################################

# Short dark triad 
n_p <- 1000    # number of people
n_i <- 27      # number of items 
n_r <- 5       # number of response options
n_t <- n_r - 1 # number of thresholds
n_d <- 3       # number of dimensions
n_c <- 2^n_d   # number of profiles  
n_id <- 9      # number of items per dimension 

# FTI 
n_p <- 2000    # number of people
n_i <- 56      # number of items 
n_r <- 4       # number of response options
n_t <- n_r - 1 # number of thresholds
n_d <- 4       # number of dimensions
n_c <- 2^n_d   # number of profiles 
n_id <- 14     # number of items per dimension 
 

# HEXACO (Humility dimension only)
n_p <- 2000    # number of people
n_i <- 40      # number of items 
n_r <- 7       # number of response options
n_t <- n_r - 1 # number of thresholds
n_d <- 4       # number of dimensions
n_c <- 2^n_d   # number of profiles
n_id <- 10     # number of items per dimension 

######################################################################################################
#Descriptive stats

d3 <- read.table('dark3.txt') # Short dark triad 
fti <- read.table('FTI.txt') # FTI
H <- read.table('H_rev.txt') # HEXACO

path <- file.path(getwd(), 'Item distributions/')
dir.create(path)

data <- fti
for (i in 1:ncol(data)) {
  p <- ggplot(data, aes(x = factor(data[[i]]))) +
       geom_bar() +
       labs(title = paste("Item", i), x = "Agreement Level", y = "Frequency") +
       theme_minimal()
  filename <- paste("item_", i, ".jpg", sep = "") 
  filepath = file.path(path, filename) 
  ggsave(filepath, plot = p, width = 7, height = 6, dpi = 500, units = "in", device='jpg')
}

#Subscales sum, max 5*9=45 point in each subscale
data$A1_Sum <- rowSums(data[, 1:9])   
data$A2_Sum <- rowSums(data[, 10:18]) 
data$A3_Sum <- rowSums(data[, 19:27]) 

#Subscale difficulty 
item_difficulty <- colMeans(data)
A1_diff <-  mean(item_difficulty[c(1:14)])
A2_diff <-  mean(item_difficulty[c(15:28)])
A3_diff <-  mean(item_difficulty[c(29:42)])
A4_diff <-  mean(item_difficulty[c(43:56)])



# Stanfit object diagnostics 

fdcm@model_pars
nrdm@model_pars
rsdm@model_pars

nrdm@date #version control

check_divergences(nrdm)

# Dark triad
rsdm_param <- c("l1I", "l2I", "l3I", "l4I", "l5I", "l6I", "l7I", "l8I", "l9I", 
                "l10I", "l11I", "l12I", "l13I", "l14I", "l15I", "l16I", "l17I", 
                "l18I", "l19I", "l20I", "l21I", "l22I", "l23I", "l24I", "l25I", 
                "l26I", "l27I", "l1M", "l2M", "l3M", "l4M", "l5M", "l6M", "l7M", 
                "l8M", "l9M", "l10M", "l11M", "l12M", "l13M", "l14M", "l15M", 
                "l16M", "l17M", "l18M", "l19M", "l20M", "l21M", "l22M", "l23M", 
                "l24M", "l25M", "l26M", "l27M", "step1_ID1", "step1_ID2", "step1_ID3", 
                "step2_ID1", "step2_ID2", "step2_ID3", "step3_ID1", "step3_ID2", 
                "step3_ID3", "step4_ID1", "step4_ID2", "step4_ID3", "step1_MD1", 
                "step1_MD2", "step1_MD3", "step2_MD1", "step2_MD2", "step2_MD3", 
                "step3_MD1", "step3_MD2", "step3_MD3", "step4_MD1", "step4_MD2", 
                "step4_MD3")

fdcm_param <- c("l1I", "l2I", "l3I", "l4I", "l5I", "l6I", "l7I", "l8I", "l9I", "l10I", 
                     "l11I", "l12I", "l13I", "l14I", "l15I", "l16I", "l17I", "l18I", "l19I", 
                     "l20I", "l21I", "l22I", "l23I", "l24I", "l25I", "l26I", "l27I", "l1M", 
                     "l2M", "l3M", "l4M", "l5M", "l6M", "l7M", "l8M", "l9M", "l10M", "l11M", 
                     "l12M", "l13M", "l14M", "l15M", "l16M", "l17M", "l18M", "l19M", "l20M", 
                     "l21M", "l22M", "l23M", "l24M", "l25M", "l26M", "l27M", "d1", "d2", "d3", 
                     "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13", "d14", 
                     "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", 
                     "d25", "d26", "d27")

# FTI

rsdm_param <-c("l1I", "l2I", "l3I", "l4I", "l5I", "l6I", "l7I", "l8I", "l9I",
                    "l10I", "l11I", "l12I", "l13I", "l14I", "l15I", "l16I", "l17I", 
                    "l18I", "l19I", "l20I", "l21I", "l22I", "l23I", "l24I", "l25I", 
                    "l26I", "l27I", "l28I", "l29I", "l30I", "l31I", "l32I", "l33I", 
                    "l34I", "l35I", "l36I", "l37I", "l38I", "l39I", "l40I", "l41I", 
                    "l42I", "l43I", "l44I", "l45I", "l46I", "l47I", "l48I", "l49I", 
                    "l50I", "l51I", "l52I", "l53I", "l54I", "l55I", "l56I", "l1M", 
                    "l2M", "l3M", "l4M", "l5M", "l6M", "l7M", "l8M", "l9M", "l10M", 
                    "l11M", "l12M", "l13M", "l14M", "l15M", "l16M", "l17M", "l18M", 
                    "l19M", "l20M", "l21M", "l22M", "l23M", "l24M", "l25M", "l26M", 
                    "l27M", "l28M", "l29M", "l30M", "l31M", "l32M", "l33M", "l34M", 
                    "l35M", "l36M", "l37M", "l38M", "l39M", "l40M", "l41M", "l42M", 
                    "l43M", "l44M", "l45M", "l46M", "l47M", "l48M", "l49M", "l50M", 
                    "l51M", "l52M", "l53M", "l54M", "l55M", "l56M", "step1_ID1", 
                    "step1_ID2", "step1_ID3", "step1_ID4", "step2_ID1", "step2_ID2", 
                    "step2_ID3", "step2_ID4", "step3_ID1", "step3_ID2", "step3_ID3", 
                    "step3_ID4", "step1_MD1", "step1_MD2", "step1_MD3", "step1_MD4", 
                    "step2_MD1", "step2_MD2", "step2_MD3", "step2_MD4", "step3_MD1", 
                    "step3_MD2", "step3_MD3", "step3_MD4")

l0 <- paste0("l0_", rep(1:n_i, times = n_t), "step", rep(1:n_t, each = n_i))
l1 <- paste0("l1_", rep(1:n_i, times = n_t), "step", rep(1:n_t, each = n_i))
nrdm_param <- c("Vc", l0, l1)


for (p in rsdm_param){
  print(traceplot(rsdm, pars = p))
}
for (p in fdcm_param){
  print(traceplot(fdcm, pars = p))
}
for (p in nrdm_param){
  print(traceplot(nrdm, pars = p))
}



# LOOIC

loglik1 <- extract(fdcm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
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
fdcm_table  <- summary(fdcm)$summary
nrdm_table  <- summary(nrdm)$summary
rsdm_table  <- summary(rsdm)$summary

# FDCM Plots

# extract the  parameters for FDCM 
#n_params <- 3 # 1)intercepts, 2) main effects, and 3) dispersion for each item
start_idx <- seq(from = n_c + 1, by = n_i, length.out = 3)
end_idx <- start_idx + n_i - 1

item_f <- data.frame(
  fdcm_table[start_idx[1]:end_idx[1], 1],
  fdcm_table[start_idx[2]:end_idx[2], 1],
  fdcm_table[start_idx[3]:end_idx[3], 1]
)

colnames(item_f) <- c("i_I","i_M", "d")
rownames(item_f) <- paste0("item_", seq(1, n_i))

# For FDCM computing item probabilities
t <- matrix(NA,n_i,n_r)

# For Short Dark Triad 
k <- 1
for(i in 1:n_i) {
  t2 <- exp(1*(item_f[i,1] + item_f[i,2]*k) + (n_r-1)*item_f[i,3])
  t3 <- exp(2*(item_f[i,1] + item_f[i,2]*k) + (n_r-2)*item_f[i,3])
  t4 <- exp(3*(item_f[i,1] + item_f[i,2]*k) + (n_r-3)*item_f[i,3])
  t5 <- exp(4*(item_f[i,1] + item_f[i,2]*k) + (n_r-4)*item_f[i,3])
  sum <- 1 + t2 + t3 + t4 + t5
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum, t5/sum)
}
t1 <- t
t <- matrix(NA,n_i,n_r)
k <- 0
t0 <- t

# For FTI
k <- 1
for(i in 1:n_i) {
  t2 <- exp(1*(item_f[i,1] + item_f[i,2]*k) + (n_r-1)*item_f[i,3])
  t3 <- exp(2*(item_f[i,1] + item_f[i,2]*k) + (n_r-2)*item_f[i,3])
  t4 <- exp(3*(item_f[i,1] + item_f[i,2]*k) + (n_r-3)*item_f[i,3])
  sum <- 1 + t2 + t3 + t4 
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum)
}
t1 <- t
t <- matrix(NA,n_i,n_r)
k <- 0
t0 <- t

# For H
k <- 1
for(i in 1:n_i) {
  t2 <- exp(1*(item_f[i,1] + item_f[i,2]*k) + (n_r-1)*item_f[i,3])
  t3 <- exp(2*(item_f[i,1] + item_f[i,2]*k) + (n_r-2)*item_f[i,3])
  t4 <- exp(3*(item_f[i,1] + item_f[i,2]*k) + (n_r-3)*item_f[i,3])
  t5 <- exp(4*(item_f[i,1] + item_f[i,2]*k) + (n_r-4)*item_f[i,3])
  t6 <- exp(5*(item_f[i,1] + item_f[i,2]*k) + (n_r-5)*item_f[i,3])
  t7 <- exp(6*(item_f[i,1] + item_f[i,2]*k) + (n_r-6)*item_f[i,3])
  sum <- 1 + t2 + t3 + t4 + t5 + t6 + t7 
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum, t5/sum, t6/sum, t7/sum)
}
t1 <- t
t <- matrix(NA,n_i,n_r)
k <- 0
t0 <- t


model.t <- rbind(t0,t1)


# For NRDM computing item probabilities
start_idx <- seq(from = n_c + 1, by = n_i, length.out = 2*n_t)
end_idx <- start_idx + n_i - 1

item_n <- data.frame(
  mapply(function(start, end) nrdm_table[start:end, 1], start_idx, end_idx)
)

colnames(item_n) <- paste0(rep(c("i", "m"), each = n_t), "_step", rep(1:n_t, 2))
rownames(item_n) <- paste0("item_", seq(1, n_i))

#For Dark Triad
t <- matrix(NA,n_i,n_r)
k <- 1
for(i in 1:n_i) {
  t2 <- exp(item_n[i,5]*k+item_n[i,1])
  t3 <- exp((item_n[i,5]+item_n[i,6])*k+item_n[i,1]+item_n[i,2])
  t4 <- exp((item_n[i,5]+item_n[i,6]+item_n[i,7])*k+item_n[i,1]+item_n[i,2]+item_n[i,3])
  t5 <- exp((item_n[i,5]+item_n[i,6]+item_n[i,7]+item_n[i,8])*k+item_n[i,1]+item_n[i,2]+item_n[i,3]+item_n[i,4])
  sum <- 1 + t2 + t3 + t4 +t5
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum, t5/sum)
}
t1 <- t
t <- matrix(NA,n_i,n_r)
k <- 0
t0 <- t

#For FTI
t <- matrix(NA,n_i,n_r)
k <- 1
for(i in 1:n_i) {
  t2 <- exp(item_n[i,4]*k+item_n[i,1])
  t3 <- exp((item_n[i,4]+item_n[i,5])*k+item_n[i,1]+item_n[i,2])
  t4 <- exp((item_n[i,4]+item_n[i,5]+item_n[i,6])*k+item_n[i,1]+item_n[i,2]+item_n[i,3])
  sum <- 1 + t2 + t3 + t4 
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum)
}
t1 <- t
t <- matrix(NA,n_i,n_r)
k <- 0
t0 <- t

#For H
t <- matrix(NA,n_i,n_r)
k <- 1
for(i in 1:n_i) {
  t2 <- exp(item_n[i,7]*k+item_n[i,1])
  t3 <- exp((item_n[i,7]+item_n[i,8])*k+item_n[i,1]+item_n[i,2])
  t4 <- exp((item_n[i,7]+item_n[i,8]+item_n[i,9])*k+item_n[i,1]+item_n[i,2]+item_n[i,3])
  t5 <- exp((item_n[i,7]+item_n[i,8]+item_n[i,9]+item_n[i,10])*k+item_n[i,1]+item_n[i,2]+item_n[i,3]+item_n[i,4])
  t6 <- exp((item_n[i,7]+item_n[i,8]+item_n[i,9]+item_n[i,10]+item_n[i,11])*k+item_n[i,1]+item_n[i,2]+item_n[i,3]+item_n[i,4]+item_n[i,5])
  t7 <- exp((item_n[i,7]+item_n[i,5]+item_n[i,9]+item_n[i,10]+item_n[i,11]+item_n[i,12])*k+item_n[i,1]+item_n[i,2]+item_n[i,3]+item_n[i,4]+item_n[i,5]+item_n[i,6])
  sum <- 1 + t2 + t3 + t4 + t5 + t6 + t7
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum, t5/sum, t6/sum, t7/sum)
}
t1 <- t
t <- matrix(NA,n_i,n_r)
k <- 0
t0 <- t


model.t <- rbind(t0,t1)



# RSDM Plots
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
step_r[4,] <- c(0.2170600, 0.310343507, 1.1467742, 0.35386807, 1.0937769, 0.08396823)
step_r <- step_r[rep(seq_len(nrow(step_r)), each = n_id), ]

colnames(item_r) <- c("i_I","i_M")
rownames(item_r) <- paste0("item_", seq(1, n_i))

colnames(step_r) <- paste0(rep(c("i", "m"), each = n_t), "_step", rep(1:n_t, 2))
rownames(step_r) <- paste("item", rep(1:n_i, 1), "_D", rep(1:n_d, each = n_id), sep="")


t <- matrix(NA,n_i,n_r)  
#For Dark Triad
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

#For FTI
k <- 1 
for(i in 1:n_i) {
  t2 <- exp((item_r[i,2]+step_r[i,4])*k+item_r[i,1]-step_r[i,1])
  t3 <- exp((item_r[i,2]+step_r[i,4]+step_r[i,5])*k+item_r[i,1]-step_r[i,1]-step_r[i,2])
  t4 <- exp((item_r[i,2]+step_r[i,4]+step_r[i,5]+step_r[i,6])*k+item_r[i,1]-step_r[i,1]-step_r[i,2]-step_r[i,3])
  t6 <- 1+t2+t3+t4
  t[i,] <- c(1/t6,t2/t6,t3/t6,t4/t6)
}
t1 <- t 
t <- matrix(NA,n_i,n_r) 
k <- 0 
t0 <- t 
model.t <- rbind(t0,t1)

#For H
for(i in 1:n_i) {
  t2 <- exp((item_r[i,2]+step_r[i,7])*k+item_r[i,1]-step_r[i,1])
  t3 <- exp((item_r[i,2]+step_r[i,7]+step_r[i,8])*k+item_r[i,1]-step_r[i,1]-step_r[i,2])
  t4 <- exp((item_r[i,2]+step_r[i,7]+step_r[i,8]+step_r[i,9])*k+item_r[i,1]-step_r[i,1]-step_r[i,2]-step_r[i,3])
  t5 <- exp((item_r[i,2]+step_r[i,7]+step_r[i,8]+step_r[i,9]+step_r[i,10])*k+item_r[i,1]-step_r[i,1]-step_r[i,2]-step_r[i,3]-step_r[i,4])
  t6 <- exp((item_r[i,2]+step_r[i,7]+step_r[i,8]+step_r[i,9]+step_r[i,10]+step_r[i,11])*k+item_r[i,1]-step_r[i,1]-step_r[i,2]-step_r[i,3]-step_r[i,4]-step_r[i,5])
  t7 <- exp((item_r[i,2]+step_r[i,7]+step_r[i,8]+step_r[i,9]+step_r[i,10]+step_r[i,11]+step_r[i,12])*k+item_r[i,1]-step_r[i,1]-step_r[i,2]-step_r[i,3]-step_r[i,4]-step_r[i,5]-step_r[i,6])
  sum <- 1 + t2 + t3 + t4 + t5 + t6 + t7
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum, t5/sum, t6/sum, t7/sum)
}
k <- 1 
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
# For Short D3
colnames(model.t) <- c("item","class", "Strongly Disagree", 
                       "Disagree", "Neutral", "Agree", "Strongly Agree")
# For FTI
colnames(model.t) <- c("item","class", "Strongly Disagree", 
                       "Disagree", "Agree", "Strongly Agree")
# For H
colnames(model.t) <- c("item","class", "Strongly Disagree", 
                       "Disagree", "Slightly Disagree", "Neutral", "Slightly Agree", 
                       "Agree", "Strongly Agree")
sapply(model.t, class)
# For  Short D3
model.t[,3:7] <- sapply(model.t[,3:7],as.numeric)
# For FTI
model.t[,3:6] <- sapply(model.t[,3:6],as.numeric)
# For H
model.t[,3:9] <- sapply(model.t[,3:9],as.numeric)
summary(model.t)

# For  Short D3
# choose from dfm_f, dfm_n, or dfm_r
dfm_f <- melt(model.t, id.vars=c("item", "class"),
              measure.vars = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
#For FTI 
dfm_r <- melt(model.t, id.vars=c("item", "class"),
              measure.vars = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))
#For H
dfm_f <- melt(model.t, id.vars=c("item", "class"),
              measure.vars = c("Strongly Disagree", 
                               "Disagree", "Slightly Disagree", "Neutral", "Slightly Agree", 
                               "Agree", "Strongly Agree"))
# save the plots for every item

path <- file.path(getwd(), 'FDCM plots/')
dir.create(path)
for (i in 1:n_i){
  item <- subset(dfm_n,item==i)
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




### Exploring malfunctioning items in RSDM

i <- 23
t2 <- exp((items[i,2]+steps_m[i,1])*k+items[i,1]-steps_i[i,1])
t3 <- exp((items[i,2]+steps_m[i,1]+steps_m[i,2])*k+items[i,1]-steps_i[i,1]-steps_i[i,2])
t4 <- exp((items[i,2]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3])*k+items[i,1]-steps_i[i,1]-steps_i[i,2]-steps_i[i,3])
t5 <- exp((items[i,2]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3]+steps_m[i,4])*k+items[i,1]-steps_i[i,1]-steps_i[i,2]-steps_i[i,3]-steps_i[i,4])
t6 <- 1+t2+t3+t4+t5
t[i,] <- c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)

# step 2 Dimension 3 too large, exploring convergence 
rsdm@model_pars
traceplot(rsdm, pars = c("step2_ID1", "step2_ID2", "step2_ID3"))
print(rsdm)



################################################################################################################
# Person level - classification agreement 
contributionsPC1 <- matrix(get_posterior_mean(fdcm,pars = c("contributionsPC"))[,3],n_p,n_c,byrow = T)
A_FDCM=unlist(lapply(1:n_p,function(x){which.max(contributionsPC1[x,])}))

contributionsPC2 <- matrix(get_posterior_mean(nrdm,pars = c("contributionsPC"))[,3],n_p,n_c,byrow = T)
A_NRDM=unlist(lapply(1:n_p,function(x){which.max(abs(contributionsPC2[x,]))}))

contributionsPC3 <- matrix(get_posterior_mean(rsdm,pars = c("contributionsPC"))[,3],n_p,n_c,byrow = T)
A_RSDM=unlist(lapply(1:n_p,function(x){which.max(contributionsPC3[x,])}))


#FDCM and NRDM

# Quick look at the results 
summary(as.factor(A_FDCM))
summary(as.factor(A_NRDM))
summary(as.factor(A_RSDM))
sum(A_FDCM==A_NRDM)/n_p
sum(A_FDCM==A_RSDM)/n_p
sum(A_NRDM==A_RSDM)/n_p

# Overlap plots for NRDM and FDCM
p <- (as.factor(A_FDCM)==as.factor(A_NRDM))*1
p <- as.data.frame(cbind(as.factor(A_NRDM), as.factor(A_FDCM), p))
p <- transform(p, V2 = as.factor(V2))

rm(p)
p <- (as.factor(A_FDCM)==as.factor(A_RSDM))*1
p <- as.data.frame(cbind(as.factor(A_RSDM), as.factor(A_FDCM), p))
p <- transform(p, V2 = as.factor(V2))
p <- transform(p, V1 = as.factor(V1))

rm(p)
p <- (as.factor(A_NRDM)==as.factor(A_RSDM))*1
p <- as.data.frame(cbind(as.factor(A_RSDM), as.factor(A_NRDM), p))
#p <- transform(p, V1 = as.factor(V1))


da <- as.data.frame(cbind(A_NRDM,A_FDCM))
#Overlap in form of table
overlap <- table(da$A_NRDM, da$A_FDCM)
print(overlap)

#For Short D3
labels <- c("000", "100",
            "010", "110", 
            "001", "101",
            "011", "111")
#For FTI and H
labels <-  c("0000", "0001",
    "0010", "0011", 
    "0100", "0101",
    "0110", "0111", 
    "1000", "1001", 
    "1010", "1011", 
    "1100", "1101",
    "1110", "1111")

model1 <- "FDCM"
model2 <- "NRDM"
models <- paste(model1,'.', model2, sep="")
data_name <- "H"

p_title <- paste(data_name,"_overlap_", models,".png", sep="")
title <- paste("Classification agreement for ", model1, " and ", model2, sep="")
  
p1 <- ggplot(p, aes(x=V2, fill=as.factor(p))) +
  geom_bar(stat="count") +
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..])) +
  scale_fill_manual(values=c("black", "grey59"),
                    name="Agreement", 
                    labels=c("Different", "Same")) +
  xlab("Attribute Profiles") + 
  ylab("Number of participants") + 
  theme(legend.position="bottom") + 
  ggtitle(title) +
  scale_x_discrete(labels=labels, guide = guide_axis(angle = 45))+
  theme_light()
p1
ggsave(p_title, plot = p1, width = 13, height = 9, dpi = 500, units = "in", device='png')



# profile differences

#NRDM and FDCM 
plot(A_NRDM, A_FDCM)
A1_NRDM <- as.matrix(A_NRDM)
A1_FDCM <- as.matrix(A_FDCM)
prf <- as.data.frame(cbind(A1_NRDM,A1_FDCM))
colnames(prf) <- c("nrdm", "fdcm")

#RSDM and FDCM 
plot(A_RSDM, A_FDCM)
A1_RSDM <- as.matrix(A_RSDM)
A1_FDCM <- as.matrix(A_FDCM)
prf <- as.data.frame(cbind(A1_RSDM,A1_FDCM))
colnames(prf) <- c("rsdm", "fdcm")

#RSDM and NRDM 
sum(A_RSDM==A_NRDM)/n_p

overlap <- table(da$A_NRDM, da$A_RSDM)
print(overlap)

plot(A_NRDM, A_RSDM)
A_NRDM <- as.matrix(A_NRDM)
A_RSDM <- as.matrix(A_RSDM)
prf <- as.data.frame(cbind(A_NRDM,A_RSDM))
colnames(prf) <- c("nrdm", "rsdm")

##############################################################################################
# Visuals for paper
path <- "C:/Users/kisle/OneDrive/Рабочий стол/FTI"
p <- ggplot(data, aes(x = factor(data[[52]]))) +
  geom_bar() +
  labs(title = "Item 52: I feel emotions more deeply than most people." , x = "Agreement Level", y = "Frequency") +
  theme_minimal()
p
filename <- paste("fti_item_52", ".jpg", sep = "") 
filepath = file.path(path, filename) 
ggsave(filepath, plot = p, width = 7, height = 6, dpi = 500, units = "in", device='jpg')

i <- 52
item_f <- subset(dfm_f,item==i)
item_r <- subset(dfm_r,item==i)
item_n <- subset(dfm_n,item==i)
p1 <- ggplot(item_f, aes(x=variable,y=value,group=class)) +
  geom_line(aes(linetype = class))+
  geom_point(aes())+
  theme_light()+
  ggtitle(paste("FDCM"))+ 
  xlab("Response options")+
  ylab("Probability")+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))
p1

p2 <- ggplot(item_n, aes(x=variable,y=value,group=class)) +
  geom_line(aes(linetype = class))+
  geom_point(aes())+
  theme_light()+
  ggtitle(paste("NRDM"))+  
  xlab("Response options")+
  ylab("Probability")+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))
p2

p3 <- ggplot(item_r, aes(x=variable,y=value,group=class)) +
  geom_line(aes(linetype = class))+
  geom_point(aes())+
  theme_light()+
  ggtitle(paste("RSDM"))+ 
  xlab("Response options")+
  ylab("Probability")+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))
p3

grid.arrange(p1, p2, p3, ncol = 3)

ggsave("fti_diff_i52.png", grid.arrange(p1, p2, p3, ncol = 3), width = 11, height = 5)


p_title2 <- paste(data_name, "_diff_", models,".png", sep="")
title2 <- paste("Classification differences between ", model1, " and ", model2, sep="")

p_dif <- ggplot(prf, aes(x=rsdm, y=fdcm))+ 
  geom_count(color='black')+
  theme_light()+
  scale_y_continuous(breaks = seq(1, n_c, by = 1), labels=labels)+
  scale_x_continuous(breaks = seq(1, n_c, by = 1), labels=labels, guide = guide_axis(angle = 45))+
  ggtitle(title2)+
  ylab(model1)+
  xlab(model2)
p_dif
ggsave(p_title2, plot = p_dif, width = 12, height = 8, dpi = 500, units = "in", device='png')













print(A4_diff)



####### Item grpahs optimization 

# seq <- seq(1, n_t)
# item_sum <- item[,1] + item[,2]*k
# t_try <- exp(outer(item_sum, seq, "*") + outer(item[,3],(n_r - seq), "*"))
# 
# summ <- 1 + apply(t_try, 1, sum)
# t_with_1 <- cbind(rep(1, n_i), t_try)
# result <- t_with_1 / summ