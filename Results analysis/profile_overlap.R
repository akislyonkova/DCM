
library(rstan)
library(rio)
library(ggplot2)
library(dplyr)

load('rsdm_2024-11-05.rda')
load('fdcm_2024-11-21.rda')

n_p <- 1000 
n_c <- 8  
n_attr <- 3

# Plots for the profile overlap 
# Quick look at the results 

contributionsPC1<-matrix(get_posterior_mean(fdcm,pars = c("contributionsPC"))[,3],n_p,n_c,byrow = T)
A_FDCM=unlist(lapply(1:n_p,function(x){which.max(contributionsPC1[x,])}))

contributionsPC2<-matrix(get_posterior_mean(rsdm,pars = c("contributionsPC"))[,3],n_p,n_c,byrow = T)
A_RSDM=unlist(lapply(1:n_p,function(x){which.max(contributionsPC2[x,])}))

summary(as.factor(A_RSDM))
summary(as.factor(A_FDCM))



#RRDM and NRDM 
sum(A_RSDM==A_FDCM)/n_p

t = (as.factor(A_FDCM)==as.factor(A_RSDM))*1
t = as.data.frame(cbind(as.factor(A_RSDM),t)) 
t = transform(t, V1 = as.factor(V1))

da = cbind(A_RSDM,A_FDCM)
with(da, table(A_RSDM,A_FDCM)) 

t(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) # profile set 

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



