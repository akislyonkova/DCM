
library(rstan)
rrdm=summary(estimated_rrdm_ordmdat)$summary
#rrdm_items = rrdm[c(17:100),c(1,3)]
#write.csv(rrdm_items,"rrdmpara.csv")
#rrdm_items = read.csv("rrdmpara.csv", header = T, row.names = 1)

# For RRDM computing item probabilities
item = data.frame(cbind(rrdm[c(17:56),1],rrdm[c(57:96),1]))
step = data.frame(rrdm[c(97:100),1])
colnames(item) = c("li_1","li_0")
colnames(step) = c("ls_0")


t=matrix(NA,40,5)
k=1
for(i in 1:40) {
  t2=exp((item[i,1]*k+item[i,2])-step[1,1])
  t3=exp(2*(item[i,1]*k+item[i,2])-step[1,1]-step[2,1])
  t4=exp(3*(item[i,1]*k+item[i,2])-step[1,1]-step[2,1]-step[3,1])
  t5=exp(4*(item[i,1]*k+item[i,2])-step[1,1]-step[2,1]-step[3,1]-step[4,1])
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}

t1=t

t=matrix(NA,40,5)
k=0
for(i in 1:40) {
  t2=exp((item[i,1]*k+item[i,2])-step[1,1])
  t3=exp(2*(item[i,1]*k+item[i,2])-step[1,1]-step[2,1])
  t4=exp(3*(item[i,1]*k+item[i,2])-step[1,1]-step[2,1]-step[3,1])
  t5=exp(4*(item[i,1]*k+item[i,2])-step[1,1]-step[2,1]-step[3,1]-step[4,1])
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t0=t
rrdm.t =rbind(t0,t1)

# Plot
rrdm.t= round(rrdm.t,4)
gr = c(rep("no attribute",40),rep("attribute",40),rep("o0",40),rep("o1",40))
id = c(rep(c(1:40),4))
rrdm.t = data.frame(cbind(id,gr,rrdm.t))
colnames(rrdm.t) = c("item","class", "SD", "D", "N", "A", "SA")

sapply(rrdm.t, class)
rrdm.t = transform(rrdm.t, SD = as.numeric(as.character(SD)))
rrdm.t = transform(rrdm.t, D = as.numeric(as.character(D)))
rrdm.t = transform(rrdm.t, N = as.numeric(as.character(N)))
rrdm.t = transform(rrdm.t, A = as.numeric(as.character(A)))
rrdm.t = transform(rrdm.t, SA = as.numeric(as.character(SA)))
summary(rrdm.t)

library(ggplot2)
library(reshape2) 

dfm <- melt(rrdm.t, id.vars=c("item", "class"),measure.vars = c("SD", "D", "N", "A", "SA"))

item=subset(dfm,item=="1")

p<-ggplot(item, aes(x=variable,y=value,group=class)) +
  geom_line(aes(color=class))+
  geom_point(aes(color=class))+
  theme_light()+
  ggtitle("Probability to select a response option (item 1)")+
  xlab("Response options")+
  ylab("Probability")
p


# For NRDM computing item probabilities

### CHANGE THIS TO MATCH THE FORMULA!
nrdm=summary(estimated_nrdm_ordmdat)$summary
maineff = data.frame(nrdm[c(17:56),1])
intercepts = data.frame(nrdm[c(57:96),1], nrdm[c(97:136),1],nrdm[c(137:176),1],nrdm[c(177:216),1])
colnames(maineff) = c("li_1")
colnames(intercepts) = c("li_01", "li_02", "li_03", "li_04")


t=matrix(NA,40,5)
k=1
for(i in 1:40) {
  t2=exp((maineff[i,1]*k+intercepts[i,1]))
  t3=exp(2(maineff[i,1]*k+intercepts[i,1])+intercepts[i,2])
  t4=exp(3(maineff[i,1]*k+intercepts[i,1])+intercepts[i,2]+intercepts[i,3])
  t5=exp(4(maineff[i,1]*k+intercepts[i,1])+intercepts[i,2]+intercepts[i,4])
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}


t1=t

t=matrix(NA,40,5)
k=0
for(i in 1:40) {
  t2=exp((maineff[i,1]*k+intercepts[i,1]))
  t3=exp(2(maineff[i,1]*k+intercepts[i,1])+intercepts[i,2])
  t4=exp(3(maineff[i,1]*k+intercepts[i,1])+intercepts[i,2]+intercepts[i,3])
  t5=exp(4(maineff[i,1]*k+intercepts[i,1])+intercepts[i,2]+intercepts[i,4])
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t0=t
nrdm.t =rbind(t0,t1)

# Plot
nrdm.t= round(nrdm.t,4)
gr = c(rep("no attribute",40),rep("attribute",40),rep("o0",40),rep("o1",40))
id = c(rep(c(1:40),4))
nrdm.t = data.frame(cbind(id,gr,nrdm.t))
colnames(nrdm.t) = c("item","class", "SD", "D", "N", "A", "SA")

sapply(nrdm.t, class)
nrdm.t = transform(nrdm.t, SD = as.numeric(as.character(SD)))
nrdm.t = transform(nrdm.t, D = as.numeric(as.character(D)))
nrdm.t = transform(nrdm.t, N = as.numeric(as.character(N)))
nrdm.t = transform(nrdm.t, A = as.numeric(as.character(A)))
nrdm.t = transform(nrdm.t, SA = as.numeric(as.character(SA)))
summary(nrdm.t)

library(ggplot2)
library(reshape2) 

dfm <- melt(nrdm.t, id.vars=c("item", "class"),measure.vars = c("SD", "D", "N", "A", "SA"))

item=subset(dfm,item=="40")

p<-ggplot(item, aes(x=variable,y=value,group=class)) +
  geom_line(aes(color=class))+
  geom_point(aes(color=class))+
  theme_light()+
  ggtitle("Probability to select a response option (item 40)")+
  xlab("Response options")+
  ylab("Probability")
p



# For RSDM computing item probabilities

rsdm=summary(estimated_rsdm_ordmdat)$summary
items = data.frame(rsdm[c(17:56),1], rsdm[c(57:96),1])
steps_i = data.frame(rsdm[c(97:100),1], rsdm[c(101:104),1],rsdm[c(105:108),1],rsdm[c(109:112),1])
steps_m = data.frame(rsdm[c(113:116),1], rsdm[c(117:120),1],rsdm[c(121:124),1],rsdm[c(125:128),1])

steps_m = read_excel("steps_m.xlsx")
steps_i = read_excel("steps_i.xlsx")

colnames(items) = c("li_0", "li_1")
colnames(steps_i) = c("ls_1", "ls_2", "ls_3", "ls_4")
colnames(steps_m) = c("ls_1", "ls_2", "ls_3", "ls_4")


t=matrix(NA,40,5)
k=1
for(i in 1:40) {
  t2=exp(((items[i,1]+steps_m[i,1])*k+items[i,2]-steps_i[i,1]))
  t3=exp(((items[i,1]+steps_m[i,1]+steps_m[i,2])*k+items[i,2]-steps_i[i,1]-steps_i[i,2]))
  t4=exp(((items[i,1]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3])*k+items[i,2]-steps_i[i,1]-steps_i[i,2]-steps_i[i,3]))
  t5=exp(((items[i,1]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3]+steps_m[i,4])*k+items[i,2]-steps_i[i,1]-steps_i[i,2]-steps_i[i,3]-steps_i[i,4]))
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}

(items[1,1]+steps_m[1,1])*1+items[1,2]-steps_i[1,1]

t1=t

t=matrix(NA,40,5)
k=0
for(i in 1:40) {
  t2=exp(((items[i,1]+steps_m[i,1])*k+items[i,2]-steps_i[i,1]))
  t3=exp(((items[i,1]+steps_m[i,1]+steps_m[i,2])*k+items[i,2]-steps_i[i,1]-steps_i[i,2]))
  t4=exp(((items[i,1]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3])*k+items[i,2]-steps_i[i,1]-steps_i[i,2]-steps_i[i,3]))
  t5=exp(((items[i,1]+steps_m[i,1]+steps_m[i,2]+steps_m[i,3]+steps_m[i,4])*k+items[i,2]-steps_i[i,1]-steps_i[i,2]-steps_i[i,3]-steps_i[i,4]))
  t6=1+t2+t3+t4+t5
  t[i,]=c(1/t6,t2/t6,t3/t6,t4/t6,t5/t6)
}
t0=t
rsdm.t=rbind(t0,t1)



# Plot
rsdm.t= round(rsdm.t,4)
gr = c(rep("no attribute",40),rep("attribute",40),rep("o0",40),rep("o1",40))
id = c(rep(c(1:40),4))
rsdm.t = data.frame(cbind(id,gr,rsdm.t))
colnames(rsdm.t) = c("item","class", "SD", "D", "N", "A", "SA")

sapply(rsdm.t, class)
rsdm.t = transform(rsdm.t, SD = as.numeric(as.character(SD)))
rsdm.t = transform(rsdm.t, D = as.numeric(as.character(D)))
rsdm.t = transform(rsdm.t, N = as.numeric(as.character(N)))
rsdm.t = transform(rsdm.t, A = as.numeric(as.character(A)))
rsdm.t = transform(rsdm.t, SA = as.numeric(as.character(SA)))
summary(rsdm.t)

library(ggplot2)
library(reshape2) 

dfm <- melt(rsdm.t, id.vars=c("item", "class"),measure.vars = c("SD", "D", "N", "A", "SA"))

item=subset(dfm,item=="40")

p<-ggplot(item, aes(x=variable,y=value,group=class)) +
  geom_line(aes(color=class))+
  geom_point(aes(color=class))+
  theme_light()+
  ggtitle("Probability to select a response option (item 40)")+
  xlab("Response options")+
  ylab("Probability")
p




# LOO
library("loo")
estimated_rrdm_ordmdat@model_pars

# Extract pointwise log-likelihood and compute LOO
log_lik_1 <- extract(estimated_rrdm_ordmdat, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff1 <- relative_eff(exp(log_lik_1)) 
loo_1 <- loo(log_lik_1, r_eff = r_eff1)
waic(log_lik_1)
print(loo_1)

# Extract pointwise log-likelihood and compute LOO
log_lik_2 <- extract(estimated_nrdm_ordmdat, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff2 <- relative_eff(exp(log_lik_2)) 
loo_2 <- loo(log_lik_2, r_eff = r_eff2)
waic(log_lik_2)
print(loo_2)


# Extract pointwise log-likelihood and compute LOO
log_lik_3 <- extract(estimated_rsdm_ordmdat, "contributionsI", permuted = F, inc_warmup = FALSE,include = TRUE)
r_eff3 <- relative_eff(exp(log_lik_3)) 
loo_3 <- loo(log_lik_3, r_eff = r_eff3)
waic(log_lik_3)
print(loo_3)



contributionsPC1<-matrix(get_posterior_mean(estimated_rrdm_ordmdat,pars = c("contributionsPC"))[,3],901,16,byrow = T)
A_RRDM=unlist(lapply(1:901,function(x){which.max(contributionsPC1[x,])}))


contributionsI2<- matrix(extract(estimated_rsdm_ordmdat,"log_lik",permuted = F, inc_warmup = FALSE,include = TRUE),901,16,byrow = T)
#contributionsPC2<-matrix(get_posterior_mean(estimated_nrdm_ordmdat,pars = c("contributionsPC"))[,3],901,16,byrow = T)
A_RSDM=unlist(lapply(1:901,function(x){which.max(contributionsI2[x,])}))

contributionsI3<- matrix(extract(estimated_rsdm_ordmdat,"log_lik",permuted = F, inc_warmup = FALSE,include = TRUE),901,16,byrow = T)
#contributionsPC2<-matrix(get_posterior_mean(estimated_nrdm_ordmdat,pars = c("contributionsPC"))[,3],901,16,byrow = T)
A_NRDM=unlist(lapply(1:901,function(x){which.max(abs(contributionsI3[x,]))}))



summary(as.factor(A_RRDM))
summary(as.factor(A_RSDM))
sum(A_RRDM==A_RSDM)/901
t = (as.factor(A_RRDM)==as.factor(A_RSDM))*1
t = as.data.frame(cbind(as.factor(A_RSDM),t)) 
t = transform(t, V1 = as.factor(V1))

da = cbind(A_RSDM,A_RRDM)
with(da, table(A_RSDM,A_RRDM)) 

library(ggplot2)
library(reshape2) 

p =ggplot(t, aes(x=V1, fill=as.factor(t))) +  geom_bar(stat="count")+geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]))+
  scale_fill_manual(values=c("#999999", "#E69F00"),
                    name="Classification Agreement", labels=c("Different", "Same"))+
xlab("Attribute Profiles") + ylab("Number of Examinees") + theme(legend.position="bottom")+ 
  scale_x_discrete(labels=c("0000" ,"0001" ,"0010" ,"0011", "0100", "0101" ,"0110" ,"0111", "1000", "1001", "1010", "1011", "1100", "1101" ,"1110", "1111"))
                                                                                                            
p

ggsave("filename.png",plot = p,width = 6, height = 6, dpi = 500, units = "in", device='png')

# Extract class definition
nc=4
temp.table.col<-unique(apply(combn(rep(c(0,1),nc),nc),2,function(x){paste(x,collapse = "")}))
temp.table.col<-temp.table.col[order(temp.table.col)]
temp.table.col

