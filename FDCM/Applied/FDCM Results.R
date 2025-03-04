library(rstan)
library(ggplot2)
library(reshape2) 
library(loo)
library(dplyr)

fdcm  <- summary(fdcm)$summary

n_i <- 27  # number of items 
n_p <- 700 # number of people
n_r <- 5   # number of response options
n_c <- 8  # number of profiles  


# extracted parameters
item <- data.frame(cbind(fdcm[c(9:35),1],fdcm[c(36:62),1]))
d <- data.frame(fdcm[c(63:89),1])
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



# Plot
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





# LOOIC
fdcm@model_pars

log_lik_1 <- extract(fdcm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff1 <- relative_eff(exp(log_lik_1)) 
loo_1 <- loo(log_lik_1, r_eff = r_eff1)
waic(log_lik_1)
print(loo_1)

