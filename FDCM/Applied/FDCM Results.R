library(rstan)
library(ggplot2)
library(reshape2) 
library(loo)
library(dplyr)

#######################################################################################################
# Short dark triad 

fdcm_table  <- summary(fdcm)$summary
nrdm_table  <- summary(nrdm)$summary

n_i <- 27  # number of items 
n_p <- 700 # number of people
n_r <- 5   # number of response options
n_c <- 8  # number of profiles  


# extracted parameters for FDCM 
item <- data.frame(cbind(fdcm_table[c(9:35),1],fdcm_table[c(36:62),1]))
d <- data.frame(fdcm_table[c(63:89),1])
colnames(item) <- c("li_I","li_M")
colnames(d) <- c("d")

# For FDCM computing item probabilities
t <- matrix(NA,n_i,n_r)
k <- 1 # the the k=1, these profiles have an attribute
for(i in 1:n_i) {
  t2 <- exp(1*(item[i,1] + item[i,2]*k) + (5-1)*d[i,])
  t3 <- exp(2*(item[i,1] + item[i,2]*k) + (5-2)*d[i,])
  t4 <- exp(3*(item[i,1] + item[i,2]*k) + (5-3)*d[i,])
  t5 <- exp(4*(item[i,1] + item[i,2]*k) + (5-4)*d[i,])
  sum <- 1 + t2 + t3 + t4 + t5
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum, t5/sum)
}
t1 <- t

# clearing the t matrix to avoid an accidental mix up
# change the k to 0, no latent attribute 
t <- matrix(NA,n_i,n_r)
k <- 0
for(i in 1:n_i) {
  t2 <- exp(1*(item[i,1] + item[i,2]*k) + (5-1)*d[i,])
  t3 <- exp(2*(item[i,1] + item[i,2]*k) + (5-2)*d[i,])
  t4 <- exp(3*(item[i,1] + item[i,2]*k) + (5-3)*d[i,])
  t5 <- exp(4*(item[i,1] + item[i,2]*k) + (5-4)*d[i,])
  sum <- 1 + t2 + t3 + t4 + t5
  t[i,] <- c(1/sum, t2/sum, t3/sum, t4/sum, t5/sum)
}
t0 <- t
fdcm.t <- rbind(t0,t1)



# FDCM Plots
fdcm.t <- round(fdcm.t,4)
gr <- c(rep("No attribute",n_i),rep("Attribute",n_i),rep("o0",n_i),rep("o1",n_i))
id <- c(rep(c(1:n_i),4))
fdcm.t <- data.frame(cbind(id,gr,fdcm.t))
colnames(fdcm.t) <- c("item","class", "Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

sapply(fdcm.t, class)
fdcm.t[,3:7] <- sapply(fdcm.t[,3:7],as.numeric)
summary(fdcm.t)


dfm_f <- melt(fdcm.t, id.vars=c("item", "class"),measure.vars = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
# save the plots for every item
path <- file.path(getwd(), 'plots/')
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
intercepts <- data.frame(lapply(list(9:35, 36:62, 63:89, 90:116), function(i) nrdm_table[i, 1]))
maineff <- data.frame(lapply(list(117:143, 144:170, 171:197, 198:224), function(i) nrdm_table[i, 1]))
colnames(maineff) <- c("M_step1", "M_step2", "M_step3", "M_step4") 
colnames(intercepts) <- c("I_step1", "I_step2", "I_step3", "I_step4") 
rm(s)


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
for(i in 1:n_i) {
  t2=exp(maineff[i,1]*k+intercepts[i,1])
  t3=exp((maineff[i,1]+maineff[i,2])*k+intercepts[i,1]+intercepts[i,2])
  t4=exp((maineff[i,1]+maineff[i,2]+maineff[i,3])*k+intercepts[i,1]+intercepts[i,2]+intercepts[i,3])
  t5=exp((maineff[i,1]+maineff[i,2]+maineff[i,3]+maineff[i,4])*k+intercepts[i,1]+intercepts[i,2]+intercepts[i,3]+intercepts[i,4])
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t0 <- t
nrdm.t <- rbind(t0,t1)



# Plot
nrdm.t <- round(nrdm.t,4)
gr <- c(rep("No attribute",n_i),rep("Attribute",n_i),rep("o0",n_i),rep("o1",n_i))
id <- c(rep(c(1:n_i),4))
nrdm.t <- data.frame(cbind(id,gr,nrdm.t))
colnames(nrdm.t) <- c("item","class", "Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

sapply(nrdm.t, class)
nrdm.t[,3:7] <- sapply(nrdm.t[,3:7],as.numeric)
summary(nrdm.t)


dfm_f <- melt(nrdm.t, id.vars=c("item", "class"),
              measure.vars = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))

# save the plots for every item
path <- file.path(getwd(), 'plots/')
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



# LOOIC
fdcm@model_pars
nrdm@model_pars

loglik1 <- extract(fdcm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff1 <- relative_eff(exp(loglik1)) 
loo1 <- loo(loglik1, r_eff = r_eff1)
print(loo_1)


loglik2 <- extract(nrdm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff2 <- relative_eff(exp(loglik2)) 
loo2 <- loo(loglik2, r_eff = r_eff2)
print(loo2)


loglik3 <- extract(rsdm, "contributionsI", permuted = F, inc_warmup = F,include = T)
r_eff3 <- relative_eff(exp(loglik3)) 
loo3 <- loo(loglik3, r_eff = r_eff3)

lpd_point <- cbind(
  loo1$pointwise[,'elpd_loo'],
  loo2$pointwise[,'elpd_loo'],
  loo3$pointwise[,'elpd_loo']
)

