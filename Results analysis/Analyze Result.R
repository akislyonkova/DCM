
library(rstan)
library(rio)
library(ggplot2)
library(reshape2) 
library(loo)
library(dplyr)

#rrdm=summary(estimated_rrdm)$summary
#nrdm=summary(estimated_nrdm)$summary
#rsdm=summary(estimated_rsdm)$summary

n_i <- 40 # number of items 
n_p <- 901 # number of people
n_r <- 5 # number of response options

disc_data <- read.csv("disc_dat.csv")
disc_data[,1:n_i] <- sapply(disc_data[,1:n_i],as.character)


for (i in 1:n_i){
  print(paste("item №",i, sep = " "))
  print(round(table(disc_data[,i]),2))
  print(round(table(disc_data[,i])/n_p*100,2))
}


# For RRDM computing item probabilities
item <- data.frame(cbind(rrdm[c(17:56),1],rrdm[c(57:96),1]))
step <- data.frame(cbind(rrdm[c(97:100),1], rrdm[c(101:104),1], rrdm[c(105:108),1], rrdm[c(109:112),1]))
colnames(item) <- c("li_I","li_M")
colnames(step) <- c("ls_1", "ls_2", "ls_3", "ls_4")
#export(item, "item.xlsx")
#export(step, "step.xlsx")

# item: rows item number, col 1 - intercept, col 2 - main effect 
# step: rows dimensions, col - step number 
# expand the step dataframe, so that every step parameter is repeated 10 times
step <- step[rep(seq_len(nrow(step)), each = 10), ]

t=matrix(NA,n_i,n_r)
k=1 # the the k=1, these profiles have an attribute
for(i in 1:n_i) {
  t2=exp((item[i,2]*k+item[i,1])+step[i,1]) # *k multiply the main effect by 1 or 0
  t3=exp(2*(item[i,2]*k+item[i,1])+step[i,1]+step[i,2]) # step[i,2] - second step for each item
  t4=exp(3*(item[i,2]*k+item[i,1])+step[i,1]+step[i,2]+step[i,3]) # item[i,2] - item main effect
  t5=exp(4*(item[i,2]*k+item[i,1])+step[i,1]+step[i,2]+step[i,3]+step[i,4]) # item[i,1] - item intercept
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t1=t

# clearing the t matrix to avoid an accidental mix up
# change the k to 0, no latent attribute 
t=matrix(NA,n_i,n_r)
k=0
for(i in 1:n_i) {
  t2=exp((item[i,2]*k+item[i,1])+step[i,1]) 
  t3=exp(2*(item[i,2]*k+item[i,1])+step[i,1]+step[i,2]) 
  t4=exp(3*(item[i,2]*k+item[i,1])+step[i,1]+step[i,2]+step[i,3]) 
  t5=exp(4*(item[i,2]*k+item[i,1])+step[i,1]+step[i,2]+step[i,3]+step[i,4]) 
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t0=t
rrdm.t =rbind(t0,t1)

# Plot
rrdm.t= round(rrdm.t,4)
gr = c(rep("no attribute",n_i),rep("attribute",n_i),rep("o0",n_i),rep("o1",n_i))
id = c(rep(c(1:n_i),4))
rrdm.t = data.frame(cbind(id,gr,rrdm.t))
colnames(rrdm.t) = c("item","class", "SD", "D", "N", "A", "SA")

sapply(rrdm.t, class)
rrdm.t[,3:7] <- sapply(rrdm.t[,3:7],as.numeric)
summary(rrdm.t)


dfm <- melt(rrdm.t, id.vars=c("item", "class"),measure.vars = c("SD", "D", "N", "A", "SA"))
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


# For NRDM computing item probabilities

intercepts = data.frame(nrdm[c(17:56),1], nrdm[c(57:96),1], nrdm[c(97:136),1],nrdm[c(137:176),1]) # extracting step parameters for each item's main effect 
maineff = data.frame(nrdm[c(177:216),1], nrdm[c(217:256),1],nrdm[c(257:296),1],nrdm[c(297:336),1]) # extracting step parameters for each item's intercept
colnames(maineff) = c("lM_step1", "lM_step2", "lM_step3", "lM_step4") # l - lambda, M - main effect 
colnames(intercepts) = c("lI_step1", "lI_step2", "lI_step3", "lI_step4") # I - intercept 


t=matrix(NA,n_i,n_r)
k=1
for(i in 1:n_i) {
  t2=exp(maineff[i,1]*k+intercepts[i,1])
  t3=exp((maineff[i,1]+maineff[i,2])*k+intercepts[i,1]+intercepts[i,2])
  t4=exp((maineff[i,1]+maineff[i,2]+maineff[i,3])*k+intercepts[i,1]+intercepts[i,2]+intercepts[i,3])
  t5=exp((maineff[i,1]+maineff[i,2]+maineff[i,3]+maineff[i,4])*k+intercepts[i,1]+intercepts[i,2]+intercepts[i,3]+intercepts[i,4])
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}


t1=t

t=matrix(NA,n_i,n_r)
k=0
for(i in 1:n_i) {
  t2=exp(maineff[i,1]*k+intercepts[i,1])
  t3=exp((maineff[i,1]+maineff[i,2])*k+intercepts[i,1]+intercepts[i,2])
  t4=exp((maineff[i,1]+maineff[i,2]+maineff[i,3])*k+intercepts[i,1]+intercepts[i,2]+intercepts[i,3])
  t5=exp((maineff[i,1]+maineff[i,2]+maineff[i,3]+maineff[i,4])*k+intercepts[i,1]+intercepts[i,2]+intercepts[i,3]+intercepts[i,4])
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t0=t
nrdm.t=rbind(t0,t1)

# Plot
nrdm.t= round(nrdm.t,n_r)
gr = c(rep("no attribute",n_i),rep("attribute",n_i),rep("o0",n_i),rep("o1",n_i))
id = c(rep(c(1:n_i),4))
nrdm.t = data.frame(cbind(id,gr,nrdm.t))
colnames(nrdm.t) = c("item","class", "SD", "D", "N", "A", "SA")

sapply(nrdm.t, class)
nrdm.t[,3:7] <- sapply(nrdm.t[,3:7],as.numeric)
summary(nrdm.t)

dfm <- melt(nrdm.t, id.vars=c("item", "class"),measure.vars = c("SD", "D", "N", "A", "SA"))

path = file.path(getwd(), 'plots/')
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




# For RSDM computing item probabilities

RSDM <- summary(estimated_rsdm)$summary

#rsdm <- read.csv("RSDMest_12Dec2023.csv") faster upload without row names 
items <- data.frame(RSDM[c(17:56),1], RSDM[c(57:96),1])
steps_i <- data.frame(RSDM[c(97:100),1], RSDM[c(101:104),1],RSDM[c(105:108),1],RSDM[c(109:112),1])
steps_m <- data.frame(RSDM[c(113:116),1], RSDM[c(117:120),1],RSDM[c(121:124),1],RSDM[c(125:128),1])
# expand the step main effects and intercept (repeat each row 10 times, # of items)
steps_i <- steps_i[rep(seq_len(nrow(steps_i)), each = 10), ]
steps_m <- steps_m[rep(seq_len(nrow(steps_m)), each = 10), ]
# label the columns 
colnames(items) <- c("li_I","li_M")
colnames(steps_i) <- c("lI_step1", "lI_step2", "lI_step3", "lI_step4")
colnames(steps_m) <- c("lM_step1", "lM_step2", "lM_step3", "lM_step4")

t=matrix(NA,n_i,n_r)  # crate a null t matrix for calculating the probabilities 
k=1 # fix the k to 1 to create t matrix for people with the latent trait 
# loop for calculting the probabilities 
for(i in 1:n_i) {
  t2 <- exp((items[i,2]+steps_m[i,1])*k+items[i,1]+steps_i[i,1])
  t3 <- exp((items[i,2]+steps_m[i,1]+steps_m[i,2])*k+items[i,2]+steps_i[i,1]+steps_i[i,2])
  t4 <- exp((items[i,2]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3])*k+items[i,1]+steps_i[i,1]+steps_i[i,2]+steps_i[i,3])
  t5 <- exp((items[i,2]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3]+steps_m[i,4])*k+items[i,1]+steps_i[i,1]+steps_i[i,2]+steps_i[i,3]+steps_i[i,4])
  t6 <- 1+t2+t3+t4+t5
  t[i,] <- c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t1 <- t # save the result for people with the latent trait 
t <- matrix(NA,n_i,n_r) # make the base t matrix null again 
k <- 0 # fix the k to 0 to create t matrix for people without the latent trait 
for(i in 1:n_i) {
  t2 <- exp((items[i,2]+steps_m[i,1])*k+items[i,1]+steps_i[i,1])
  t3 <- exp((items[i,2]+steps_m[i,1]+steps_m[i,2])*k+items[i,2]+steps_i[i,1]+steps_i[i,2])
  t4 <- exp((items[i,2]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3])*k+items[i,1]+steps_i[i,1]+steps_i[i,2]+steps_i[i,3])
  t5 <- exp((items[i,2]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3]+steps_m[i,4])*k+items[i,1]+steps_i[i,1]+steps_i[i,2]+steps_i[i,3]+steps_i[i,4])
  t6 <- 1+t2+t3+t4+t5
  t[i,] <- c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t0 <- t # save the result for people with the latent trait 
rsdm.t <- rbind(t0,t1) # combine the results into one matrix 

# Plot
rsdm.t <- round(rsdm.t,4)
gr <- c(rep("no attribute",n_i),rep("attribute",n_i),rep("o0",n_i),rep("o1",n_i))
id <- c(rep(c(1:n_i),4))
rsdm.t <- data.frame(cbind(id,gr,rsdm.t))
colnames(rsdm.t) <- c("item","class", "SD", "D", "N", "A", "SA")
rsdm.t[,3:7] <- sapply(rsdm.t[,3:7],as.numeric) # change the rype of var for response option probabilities 
summary(rsdm.t)

dfm <- melt(rsdm.t, id.vars=c("item", "class"),measure.vars = c("SD", "D", "N", "A", "SA"))

path = file.path(getwd(), 'plots/')
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




# LOO
estimated_rrdm@model_pars

# RRDM
log_lik_1 <- extract(estimated_rrdm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff1 <- relative_eff(exp(log_lik_1)) 
loo_1 <- loo(log_lik_1, r_eff = r_eff1)
waic(log_lik_1)
print(loo_1)

# NRDM
log_lik_2 <- extract(estimated_nrdm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff2 <- relative_eff(exp(log_lik_2)) 
loo_2 <- loo(log_lik_2, r_eff = r_eff2)
waic(log_lik_2)
print(loo_2)


# RSDM
log_lik_3 <- extract(rsdm, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff3 <- relative_eff(exp(log_lik_3)) 
loo_3 <- loo(log_lik_3, r_eff = r_eff3)
waic(log_lik_3)
print(loo_3)



contributionsPC1<-matrix(get_posterior_mean(estimated_rrdm,pars = c("contributionsPC"))[,3],901,16,byrow = T)
#contributionsPC1 <-read.csv("ContributionsPC_RRDM.csv")
A_RRDM=unlist(lapply(1:n_p,function(x){which.max(contributionsPC1[x,])}))

contributionsPC2<-matrix(get_posterior_mean(estimated_rsdm,pars = c("contributionsPC"))[,3],901,16,byrow = T)
A_RSDM=unlist(lapply(1:n_p,function(x){which.max(contributionsPC2[x,])}))

contributionsPC3<-matrix(get_posterior_mean(estimated_nrdm,pars = c("contributionsPC"))[,3],901,16,byrow = T)
A_NRDM=unlist(lapply(1:n_p,function(x){which.max(abs(contributionsPC3[x,]))}))




# Plots for the profile overlap 
# Quick look at the results 
summary(as.factor(A_RRDM))
summary(as.factor(A_RSDM))
summary(as.factor(A_NRDM))

#RRDM and RSDM 
sum(A_RRDM==A_RSDM)/n_p
t = (as.factor(A_RRDM)==as.factor(A_RSDM))*1
t = as.data.frame(cbind(as.factor(A_RSDM),t)) 
t = transform(t, V1 = as.factor(V1))

da = cbind(A_RSDM,A_RRDM)
with(da, table(A_RSDM,A_RRDM)) 


p <- ggplot(t, aes(x=V1, fill=as.factor(t))) +
  geom_bar(stat="count") +
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..])) +
  scale_fill_manual(values=c("#999999", "#E69F00"),name="Classification Agreement", labels=c("Different", "Same"))+
  xlab("Attribute Profiles") + 
  ylab("Number of Examinees") + 
  theme(legend.position="bottom")+ 
  scale_x_discrete(labels=c("0000" ,"0001" ,"0010" ,"0011", "0100", "0101" ,"0110" ,"0111", "1000", "1001", "1010", "1011", "1100", "1101" ,"1110", "1111"))

p

ggsave("RRDM_RSDM_profile_overlap.png",plot = p,width = 6, height = 6, dpi = 500, units = "in", device='png')


#RRDM and NSDM 
sum(A_RRDM==A_NRDM)/n_p
t = (as.factor(A_RRDM)==as.factor(A_NRDM))*1
t = as.data.frame(cbind(as.factor(A_NRDM),t)) 
t = transform(t, V1 = as.factor(V1))

da = cbind(A_NRDM,A_RRDM)
with(da, table(A_NRDM,A_RRDM)) 


p <- ggplot(t, aes(x=V1, fill=as.factor(t))) +
  geom_bar(stat="count") +
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..])) +
  scale_fill_manual(values=c("#6699FF", "#99CCff"),
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


p

ggsave("RRDM_NRDM_profile_overlap.png",plot = p,width = 10, height = 6, dpi = 500, units = "in", device='png')


# Simulation part 
rrdm <- read.table("RRDMest_15Nov2023.txt") # uploading the original estimated model 
rrdm_param <- data.frame(rrdm[c(rows_start:rows_end),1]) # selecting the parameters 

n_sim <- 10  # number of datasets on simulated data
rows_start <- 17 # location of the first parameter 
rows_end <- 112  # location of the second parameter 


sim_param <- NA # placeholder for uploading the simulated parameters 
for (i in 1:n_sim) { # loop for uploading and selecting the correct parameters for n simulations 
  sim_name <- sprintf("RRDMsim%i.txt", i)
  sim_param <- cbind(sim_param,read.table(sim_name)[c(rows_start:rows_end),1])
}
sim_param <- sim_param[,-1] # deleting the first NA column 

rrdm_param_n <- data.frame(rep(rrdm_param, n_sim)) # duplicating the columns with the original parameters
sim_param_dif <- rrdm_param_n - sim_param # calulating the differences 
bias <- data.frame(rowSums(sim_param_dif)/n_sim) # calculating raw bias 

write.table(bias, "RRDM_bias.txt") # saving the results 














