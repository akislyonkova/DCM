library(rstan)
library(ggplot2)
library(reshape2) 
library(loo)
library(dplyr)

#######################################################################################################
n_p <- 1000    # number of people

# Short dark triad 
n_i <- 27      # number of items 
n_r <- 5       # number of response options
n_t <- n_r - 1 # number of thresholds
n_d <- 3       # number of dimensions
n_c <- 2^n_d   # number of profiles  
n_id <- 9      # number of items per dimension 

# FTI 
n_i <- 56      # number of items 
n_r <- 4       # number of response options
n_t <- n_r - 1 # number of thresholds
n_d <- 4       # number of dimensions
n_c <- 2^n_d   # number of profiles 
n_id <- 14     # number of items per dimension 
 

# HEXACO (Humility dimension only)
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
H <- read.table('H.txt') # HEXACO

path <- file.path(getwd(), 'Item distributions/')
dir.create(path)

data <- H
for (i in 1:ncol(data)) {
  p <- ggplot(data, aes(x = factor(data[[i]]))) +
       geom_bar() +
       labs(title = paste("Item", i), x = "Agreement Level", y = "Frequency") +
       theme_minimal()
  #print(p)
  filename <- paste("item_", i, ".jpg", sep = "") 
  filepath = file.path(path, filename) 
  ggsave(filepath, plot = p, width = 7, height = 6, dpi = 500, units = "in", device='jpg')
}

# Stanfit object diagnostics 

check_divergences(fdcm)
traceplot(fdcm, pars = c("l32M", "l34M", "l35M"))



# LOOIC
fdcm@model_pars
nrdm@model_pars
rsdm@model_pars

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
n_params <- 3 # 1)intercepts, 2) main effects, and 3) dispersion for each item
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
  t2 <- exp(1*(item[i,1] + item[i,2]*k) + (n_r-1)*item[i,3])
  t3 <- exp(2*(item[i,1] + item[i,2]*k) + (n_r-2)*item[i,3])
  t4 <- exp(3*(item[i,1] + item[i,2]*k) + (n_r-3)*item[i,3])
  t5 <- exp(4*(item[i,1] + item[i,2]*k) + (n_r-4)*item[i,3])
  sum <- 1 + t2 + t3 + t4 + t5
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum, t5/sum)
}
t1 <- t
t0 <- t
fdcm.t <- rbind(t0,t1)

# For FTI
k <- 1
for(i in 1:n_i) {
  t2 <- exp(1*(item[i,1] + item[i,2]*k) + (n_r-1)*item[i,3])
  t3 <- exp(2*(item[i,1] + item[i,2]*k) + (n_r-2)*item[i,3])
  t4 <- exp(3*(item[i,1] + item[i,2]*k) + (n_r-3)*item[i,3])
  sum <- 1 + t2 + t3 + t4 
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum)
}
t1 <- t
t <- matrix(NA,n_i,n_r)
k <- 0
t0 <- t
fdcm.t <- rbind(t0,t1)


# seq <- seq(1, n_t)
# item_sum <- item[,1] + item[,2]*k
# t_try <- exp(outer(item_sum, seq, "*") + outer(item[,3],(n_r - seq), "*"))
# 
# summ <- 1 + apply(t_try, 1, sum)
# t_with_1 <- cbind(rep(1, n_i), t_try)
# result <- t_with_1 / summ

fdcm.t <- round(fdcm.t,4)
gr <- c(rep("No attribute",n_i),rep("Attribute",n_i),rep("o0",n_i),rep("o1",n_i))
id <- c(rep(c(1:n_i),4))
fdcm.t <- data.frame(cbind(id,gr,fdcm.t))
colnames(fdcm.t) <- c("item","class", "Strongly Disagree", 
                      "Disagree", "Neutral", "Agree", "Strongly Agree")
# For FTI
colnames(fdcm.t) <- c("item","class", "Strongly Disagree", 
                      "Disagree", "Agree", "Strongly Agree")
sapply(fdcm.t, class)
# For FTI
fdcm.t[,3:6] <- sapply(fdcm.t[,3:6],as.numeric)
summary(fdcm.t)


dfm_f <- melt(fdcm.t, id.vars=c("item", "class"),
              measure.vars = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
#For FTI 
dfm_f <- melt(fdcm.t, id.vars=c("item", "class"),
              measure.vars = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))
# save the plots for every item

path <- file.path(getwd(), 'FDCM plots/')
dir.create(path)
for (i in 1:n_i){
  item <- subset(dfm_f,item==i)
  p <- ggplot(item, aes(x=variable,y=value,group=class)) +
    geom_line(aes(color=class))+
    geom_point(aes(color=class))+
    theme_light()+
    ggtitle(paste("Probability to select a response option, item", i))+ 
    xlab("Response options")+
    ylab("Probability")
  filename <- paste("item_", i, ".png", sep = "") 
  filepath = file.path(path, filename) 
  ggsave(filepath, plot = p, width = 7, height = 6, dpi = 500, units = "in", device='png')
}


# For NRDM computing item probabilities
start_idx <- seq(from = n_c + 1, by = n_i, length.out = 2*n_t)
end_idx <- start_idx + n_i - 1

item_n <- data.frame(
  mapply(function(start, end) nrdm_table[start:end, 1], start_idx, end_idx)
)

colnames(item_n) <- paste0(rep(c("i", "m"), each = n_t), "_step", rep(1:n_t, 2))
rownames(item_n) <- paste0("item_", seq(1, n_i))


t <- matrix(NA,n_i,n_r)
k <- 1
for(i in 1:n_i) {
  t2=exp(maineff[i,1]*k+intercepts[i,1])
  t3=exp((maineff[i,1]+maineff[i,2])*k+intercepts[i,1]+intercepts[i,2])
  t4=exp((maineff[i,1]+maineff[i,2]+maineff[i,3])*k+intercepts[i,1]+intercepts[i,2]+intercepts[i,3])
  t5=exp((maineff[i,1]+maineff[i,2]+maineff[i,3]+maineff[i,4])*k+intercepts[i,1]+intercepts[i,2]+intercepts[i,3]+intercepts[i,4])
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t1 <- t
t <- matrix(NA,n_i,n_r)
k <- 0
t0 <- t
nrdm.t <- rbind(t0,t1)

#For FTI
t <- matrix(NA,n_i,n_r)
k <- 1
for(i in 1:n_i) {
  t2=exp(item_n[i,4]*k+item_n[i,1])
  t3=exp((item_n[i,4]+item_n[i,5])*k+item_n[i,1]+item_n[i,2])
  t4=exp((item_n[i,4]+item_n[i,5]+item_n[i,6])*k+item_n[i,1]+item_n[i,2]+item_n[i,3])
  t6=1+t2+t3+t4
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6)
}
t1 <- t
t <- matrix(NA,n_i,n_r)
k <- 0
t0 <- t
nrdm.t <- rbind(t0,t1)

# NRDM Item Plots
nrdm.t <- round(nrdm.t,4)
gr <- c(rep("No attribute",n_i),rep("Attribute",n_i),rep("o0",n_i),rep("o1",n_i))
id <- c(rep(c(1:n_i),4))
nrdm.t <- data.frame(cbind(id,gr,nrdm.t))

#For FTI
colnames(nrdm.t) <- c("item","class", "Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
sapply(nrdm.t, class)
nrdm.t[,3:6] <- sapply(nrdm.t[,3:6],as.numeric)
summary(nrdm.t)

dfm_f <- melt(nrdm.t, id.vars=c("item", "class"),
              measure.vars = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
#For FTI
dfm_f <- melt(nrdm.t, id.vars=c("item", "class"),
              measure.vars = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))
path <- file.path(getwd(), 'NRDM plots/')
dir.create(path)
for (i in 1:n_i){
  item <- subset(dfm_f,item==i)
  p <- ggplot(item, aes(x=variable,y=value,group=class)) +
    geom_line(aes(color=class))+
    geom_point(aes(color=class))+
    theme_light()+
    ggtitle(paste("Probability to select a response option, item", i))+ 
    xlab("Response options")+
    ylab("Probability")
  filename <- paste("item_", i, ".png", sep = "") 
  filepath = file.path(path, filename) 
  ggsave(filepath, plot = p, width = 7, height = 6, dpi = 500, units = "in", device='png')
}

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
step_r <- step_r[rep(seq_len(nrow(step_r)), each = n_id), ]

colnames(item_r) <- c("i_I","i_M")
rownames(item_r) <- paste0("item_", seq(1, n_i))

colnames(step_r) <- paste0(rep(c("i", "m"), each = n_t), "_step", rep(1:n_t, 2))
rownames(step_r) <- paste("item", rep(1:n_i, 1), "_D", rep(1:n_d, each = n_id), sep="")


t <- matrix(NA,n_i,n_r)  
#For Dark Triad
for(i in 1:n_i) {
  t2 <- exp((item_r[i,2]+steps_m[i,1])*k+item_r[i,1]-steps_i[i,1])
  t3 <- exp((item_r[i,2]+steps_m[i,1]+steps_m[i,2])*k+item_r[i,1]-steps_i[i,1]-steps_i[i,2])
  t4 <- exp((item_r[i,2]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3])*k+item_r[i,1]-steps_i[i,1]-steps_i[i,2]-steps_i[i,3])
  t5 <- exp((item_r[i,2]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3]+steps_m[i,4])*k+item_r[i,1]-steps_i[i,1]-steps_i[i,2]-steps_i[i,3]-steps_i[i,4])
  t6 <- 1+t2+t3+t4+t5
  t[i,] <- c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}

#For FTI
for(i in 1:n_i) {
  t2 <- exp((item_r[i,2]+step_r[i,4])*k+item_r[i,1]-step_r[i,1])
  t3 <- exp((item_r[i,2]+step_r[i,4]+step_r[i,5])*k+item_r[i,1]-step_r[i,1]-step_r[i,2])
  t4 <- exp((item_r[i,2]+step_r[i,4]+step_r[i,5]+step_r[i,6])*k+item_r[i,1]-step_r[i,1]-step_r[i,2]-step_r[i,3])
  t6 <- 1+t2+t3+t4
  t[i,] <- c(1/t6,t2/t6,t3/t6,t4/t6)
}

k <- 1 
t1 <- t 
t <- matrix(NA,n_i,n_r) 
k <- 0 
t0 <- t 
rsdm.t <- rbind(t0,t1) 






# RSDM Plots
rsdm.t <- round(rsdm.t,4)
gr <- c(rep("no attribute",n_i),rep("attribute",n_i),rep("o0",n_i),rep("o1",n_i))
id <- c(rep(c(1:n_i),4))
rsdm.t <- data.frame(cbind(id,gr,rsdm.t))

#For Dark Triad
colnames(rsdm.t) <- c("item","class", "Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
rsdm.t[,3:7] <- sapply(rsdm.t[,3:7],as.numeric) 
dfm <- melt(rsdm.t, id.vars=c("item", "class"),
            measure.vars = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))

#For FTI
colnames(rsdm.t) <- c("item","class", "Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
rsdm.t[,3:6] <- sapply(rsdm.t[,3:6],as.numeric) 
dfm <- melt(rsdm.t, id.vars=c("item", "class"),
            measure.vars = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))


path <- file.path(getwd(), 'RSDM plots/')
dir.create(path)
# save the plots for every item
for (i in 1:n_i){
  item <- subset(dfm,item==i)
  p <- ggplot(item, aes(x=variable,y=value,group=class)) +
    geom_line(aes(color=class))+
    geom_point(aes(color=class))+
    theme_light()+
    ggtitle(paste("Probability to select a response option, item", i))+ # use paste for ggtitle
    xlab("Response options")+
    ylab("Probability")
  filename <- paste("item_", i, ".png", sep = "") # creates the file name for each plot
  filepath = file.path(path, filename) # creates the path for each plot
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
# Profile Overlap 
contributionsPC1<-matrix(get_posterior_mean(fdcm,pars = c("contributionsPC"))[,3],n_p,n_c,byrow = T)
A_FDCM=unlist(lapply(1:n_p,function(x){which.max(contributionsPC1[x,])}))

contributionsPC2<-matrix(get_posterior_mean(nrdm,pars = c("contributionsPC"))[,3],n_p,n_c,byrow = T)
A_NRDM=unlist(lapply(1:n_p,function(x){which.max(abs(contributionsPC2[x,]))}))

contributionsPC3<-matrix(get_posterior_mean(rsdm,pars = c("contributionsPC"))[,3],n_p,n_c,byrow = T)
A_RSDM=unlist(lapply(1:n_p,function(x){which.max(contributionsPC3[x,])}))


#FDCM and NRDM 

# Quick look at the results 
summary(as.factor(A_FDCM))
summary(as.factor(A_NRDM))
sum(A_FDCM==A_NRDM)/n_p

# Overlap plots for NRDM and FDCM
t <- (as.factor(A_FDCM)==as.factor(A_NRDM))*1
t <- as.data.frame(cbind(as.factor(A_NRDM),t))
t <- transform(t, V1 = as.factor(V1))

da <- as.data.frame(cbind(A_NRDM,A_FDCM))
#Overlap in form of table
overlap <- table(da$A_NRDM, da$A_FDCM)
print(overlap)

#For FTI
p1 <- ggplot(t, aes(x=V1, fill=as.factor(t))) +
  geom_bar(stat="count") +
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..])) +
  scale_fill_manual(values=c("black", "grey59"),
                    name="Classification Agreement", 
                    labels=c("Different", "Same"))+
  xlab("Attribute Profiles") + 
  ylab("Number of Examinees") + 
  theme(legend.position="bottom")+ 
  scale_x_discrete(labels=c("0000", "0001",
                            "0010", "0011", 
                            "0100", "0101",
                            "0110", "0111", 
                            "1000", "1001", 
                            "1010", "1011", 
                            "1100", "1101",
                            "1110", "1111"))+
  theme_light()
p1
ggsave("FDCM_NRDM_FTI_overlap.png",plot = p1,width = 10, height = 6, dpi = 500, units = "in", device='png')

# profile differences
plot(A_NRDM, A_FDCM)
A_NRDM <- as.matrix(A_NRDM)
A_FDCM <- as.matrix(A_FDCM)
prf <- as.data.frame(cbind(A_NRDM,A_FDCM))
colnames(prf) <- c("nrdm", "fdcm")
p2 <- ggplot(prf, aes(x=nrdm, y=fdcm))+ 
  geom_count(color='black')+
  theme_light()+
  scale_y_continuous(breaks = seq(1, n_c, by = 1), labels=c("0000", "0001",
                                                          "0010", "0011", 
                                                          "0100", "0101",
                                                          "0110", "0111", 
                                                          "1000", "1001", 
                                                          "1010", "1011", 
                                                          "1100", "1101",
                                                          "1110", "1111"))+
  scale_x_continuous(breaks = seq(1, n_c, by = 1), labels=c("0000", "0001",
                                                          "0010", "0011", 
                                                          "0100", "0101",
                                                          "0110", "0111", 
                                                          "1000", "1001", 
                                                          "1010", "1011", 
                                                          "1100", "1101",
                                                          "1110", "1111"))+
  ggtitle('Profile differences between NRDM and FDCM')+
  ylab('FDCM classification')+
  xlab('NRDM classification')
p2
ggsave("FDCM_NRDM_FTI_diff.png",plot = p2,width = 10, height = 6, dpi = 500, units = "in", device='png')


#RSDM and FDCM 
sum(A_RSDM==A_FDCM)/n_p

t <- (as.factor(A_FDCM)==as.factor(A_RSDM))*1
t <- as.data.frame(cbind(as.factor(A_RSDM),t))
t <- transform(t, V1 = as.factor(V1))

da <- as.data.frame(cbind(A_RSDM,A_FDCM))
overlap <- table(da$A_RSDM, da$A_FDCM)
print(overlap)

#t(expand.grid(replicate(3, 0:1, simplify = FALSE))) # profile set 

p1 <- ggplot(t, aes(x=V1, fill=as.factor(t))) +
  geom_bar(stat="count") +
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..])) +
  scale_fill_manual(values=c("black", "grey59"),
                    name="Classification Agreement", 
                    labels=c("Different", "Same"))+
  xlab("Attribute Profiles") + 
  ylab("Number of Examinees") + 
  theme(legend.position="bottom")+ 
  scale_x_discrete(labels=c("000", "100",
                            "010", "110", 
                            "001", "101",
                            "011", "111"))+
  theme_light()
p1

#ggsave("RRDM_NRDM_profile_overlap.png",plot = p1,width = 10, height = 6, dpi = 500, units = "in", device='png')


# profile differences 
plot(A_RSDM, A_FDCM)
A_RSDM <- as.matrix(A_RSDM)
A_FDCM <- as.matrix(A_FDCM)
prf <- as.data.frame(cbind(A_RSDM,A_FDCM))
colnames(prf) <- c("rsdm", "fdcm")
p2 <- ggplot(prf, aes(x=rsdm, y=fdcm))+ 
  geom_count(color='black')+
  theme_light()+
  scale_y_continuous(breaks = seq(1, 8, by = 1), labels=c("000", "100",
                                                          "010", "110", 
                                                          "001", "101",
                                                          "011", "111"))+
  scale_x_continuous(breaks = seq(1, 8, by = 1), labels=c("000", "100",
                                                          "010", "110", 
                                                          "001", "101",
                                                          "011", "111"))+
  ggtitle('Profile differences between RSDM and FDCM')+
  ylab('FDCM classification')+
  xlab('RSDM classification')
p2
#ggsave("RRDM_NRDM_diff.png",plot = p2,width = 10, height = 9, dpi = 500, units = "in", device='png')

#RSDM and NRDM 
sum(A_RSDM==A_NRDM)/n_p

t <- (as.factor(A_RSDM)==as.factor(A_NRDM))*1
t <- as.data.frame(cbind(as.factor(A_NRDM),t))
t <- transform(t, V1 = as.factor(V1))

da <- as.data.frame(cbind(A_NRDM,A_RSDM))
#Overlap in form of table
overlap <- table(da$A_NRDM, da$A_RSDM)
print(overlap)


p1 <- ggplot(t, aes(x=V1, fill=as.factor(t))) +
  geom_bar(stat="count") +
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..])) +
  scale_fill_manual(values=c("black", "grey59"),
                    name="Classification Agreement", 
                    labels=c("Different", "Same"))+
  xlab("Attribute Profiles") + 
  ylab("Number of Examinees") + 
  theme(legend.position="bottom")+ 
  scale_x_discrete(labels=c("0000", "0001",
                            "0010", "0011", 
                            "0100", "0101",
                            "0110", "0111", 
                            "1000", "1001", 
                            "1010", "1011", 
                            "1100", "1101",
                            "1110", "1111"))+
  theme_light()
p1

# profile differences 
plot(A_NRDM, A_RSDM)
A_NRDM <- as.matrix(A_NRDM)
A_RSDM <- as.matrix(A_RSDM)
prf <- as.data.frame(cbind(A_NRDM,A_RSDM))
colnames(prf) <- c("nrdm", "rsdm")
p2 <- ggplot(prf, aes(x=nrdm, y=rsdm))+ 
  geom_count(color='black')+
  theme_light()+
  scale_y_continuous(breaks = seq(1, 8, by = 1), labels=c("000", "100",
                                                          "010", "110", 
                                                          "001", "101",
                                                          "011", "111"))+
  scale_x_continuous(breaks = seq(1, 8, by = 1), labels=c("000", "100",
                                                          "010", "110", 
                                                          "001", "101",
                                                          "011", "111"))+
  ggtitle('Profile differences between NRDM and FDCM')+
  ylab('RSDM classification')+
  xlab('NRDM classification')
p2
