# Simulate datasets 

set.seed(2025)
N <- 1000 
i <- 27  
s <- 4


# function to generate data
responses <- list() # creates an empty list for holding responses 
gendata_fdcm <- function (n_dataset, item, alpha, Q, d) {
  for (n in 1:n_dataset){     # n - number of datasets 
    for(k in 1:nrow(alpha)) { # k = N 
      for(i in 1:9) {        # i - item, questions 1 through 10 (1st attribute) 
        t1=exp(1*(item[i,1] + item[i,2]*alpha[k,1]) + (5-1)*d[i,])
        t2=exp(2*(item[i,1] + item[i,2]*alpha[k,1]) + (5-2)*d[i,])
        t3=exp(3*(item[i,1] + item[i,2]*alpha[k,1]) + (5-3)*d[i,])
        t4=exp(4*(item[i,1] + item[i,2]*alpha[k,1]) + (5-4)*d[i,])
        sum=1+t1+t2+t3+t4
        t[i,] = c(1/sum, t1/sum,t2/sum,t3/sum, t4/sum)
        ppp=rmultinom(n=1, size=1, prob=t[i,]) # n- number of random vectors, prob - probabilities sum=1
        r[k,i]=which(ppp == 1, arr.ind=TRUE)[1] #which() function returns the position/index of the value
      }
      for(i in 10:18) { # questions 11 through 20 (2nd attribute)
        t1=exp(1*(item[i,1] + item[i,2]*alpha[k,2]) + (5-1)*d[i,])
        t2=exp(2*(item[i,1] + item[i,2]*alpha[k,2]) + (5-2)*d[i,])
        t3=exp(3*(item[i,1] + item[i,2]*alpha[k,2]) + (5-3)*d[i,])
        t4=exp(4*(item[i,1] + item[i,2]*alpha[k,2]) + (5-4)*d[i,])
        sum=1+t1+t2+t3+t4
        t[i,]=c(1/sum,t1/sum,t2/sum,t3/sum, t4/sum)
        ppp=rmultinom(n=1, size=1, prob=t[i,]) 
        r[k,i]=which(ppp == 1, arr.ind=TRUE)[1] 
      }
      for(i in 19:27) { # questions 19 through 27 (3rd attribute)
        t1=exp(1*(item[i,1] + item[i,2]*alpha[k,3]) + (5-1)*d[i,])
        t2=exp(2*(item[i,1] + item[i,2]*alpha[k,3]) + (5-2)*d[i,])
        t3=exp(3*(item[i,1] + item[i,2]*alpha[k,3]) + (5-3)*d[i,])
        t4=exp(4*(item[i,1] + item[i,2]*alpha[k,3]) + (5-4)*d[i,])
        sum=1+t1+t2+t3+t4
        t[i,]=c(1/sum,t1/sum,t2/sum,t3/sum, t4/sum)
        ppp=rmultinom(n=1, size=1, prob=t[i,])
        r[k,i]=which(ppp == 1, arr.ind=TRUE)[1]
      }
    }
    responses[[length(responses)+1]] = as.data.frame(r)
  }
  responses
}

t <- matrix(NA, i, s+1)
r <- matrix(NA, N, i) # empty response matrix

Q=matrix(c(rep(c(1,0,0),9),rep(c(0,1,0),9),
           rep(c(0,0,1),9)), 27, 3, byrow = T) # 27 questions and 3 attributes
colnames(Q) <- c('A1','A2', 'A3')
n_attr <- dim(Q)[2]

#define alpha pattern
alpha.patt <- expand.grid(replicate(n_attr, 0:1, simplify = F)) # profile set
AP <- nrow(alpha.patt) # extracts the number of profiles
x <- runif(2^n_attr, min = 0, max = 1) # generate 2^A random values between 1 and 0
alpha.prob <- x/sum(x) # normalize this vector so that the sum is equal to 1
ind <- sample(x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ind, ] # simulated pattern ("truth") for all attributes for each person

###  Generating the intercepts parameters
item_i <- matrix(runif(i, min = -1, max = 1), i, 1, byrow = T) 

###########################################################################################################

### Generating cell 1 large main effects and large dispersion 

### Large main effects: min = 0.9, max = 3

item_m_large <- matrix(runif(i, min = 0.9, max = 3), i, 1, byrow = T) 
item_large <- cbind(item_i, item_m_large)
item_large <- as.data.frame(round(item_large,4))
colnames(item_large) <- c('I', 'M')

###  Large dispersion parameters: min = 0.9, max = 2
d_large <- matrix(runif(i, min = 0.9, max = 2), i, 1, byrow = T)
d_large <- round(d_large,4)
colnames(d_large) <- 'd'

# Saving the parameters used for generating the data
item_unlist_large <- as.data.frame(unlist(item_large, use.names = T))
d_param_large <- as.data.frame(unlist(d_large, use.names = T))
d_unlist_large <- as.data.frame(unlist(d_param_large, use.names = T))
colnames(item_unlist_large) <- 'V1'
colnames(d_unlist_large) <- 'V1'
cell1_param <- rbind(item_unlist_large, d_unlist_large)

write.table(cell1_param, file = 'cell1_param.txt') # save cell 1 params

cell1 <- gendata_fdcm(n_dataset = 20, alpha = alpha, item = item_large,  d = d_large) # generate data
save(cell1, file = 'fdcm_cell1.rda') # saves generated cell 1 

##########################################################################################################

### Generating cell 2: large main effects and small  dispersion 

d_large[,]
item_large[,2]

###  Generating the dispersion parameters: min = 0.1, max = 0.9
d_small <- matrix(runif(i, min = 0.1, max = 0.9), i, 1, byrow = T)
d_small <- round(d_small,4)
colnames(d_small) <- 'd'
d_param_small <- as.data.frame(unlist(d_small, use.names = T))
d_unlist_small <- as.data.frame(unlist(d_param_small, use.names = T))
colnames(d_unlist_small) <- 'V1'
cell2_param <- rbind(item_unlist_large, d_unlist_small)

write.table(cell2_param, file = 'cell2_param.txt') 

cell2 <- gendata_fdcm(n_dataset = 20, alpha = alpha, item = item_large,  d = d_small) 
save(cell2, file = 'fdcm_cell2.rda') 

##########################################################################################################

### Generating cell 3: small main effects and  large dispersion 
item_large[,2]

item_m_small <- matrix(runif(i, min = 0.1, max = 0.9), i, 1, byrow = T) 
item_small <- cbind(item_i, item_m_small)
item_small <- as.data.frame(round(item_small,4))
colnames(item_small) <- c('I', 'M')

item_unlist_small <- as.data.frame(unlist(item_small, use.names = T))
colnames(item_unlist_small) <- 'V1'
cell3_param <- rbind(item_unlist_small, d_unlist_large)

write.table(cell3_param, file = 'cell3_param.txt') 

item_small[,2]
d_large[,]

cell3 <- gendata_fdcm(n_dataset = 20, alpha = alpha, item = item_small,  d = d_large)
save(cell3, file = 'fdcm_cell3.rda') 

##########################################################################################################

### Generating cell 4: small main effects and  small dispersion 

cell4_param <- rbind(item_unlist_small, d_unlist_small)

write.table(cell4_param, file = 'cell4_param.txt') 

item_small[,2]
d_small[,]

cell4 <- gendata_fdcm(n_dataset = 20, alpha = alpha, item = item_small,  d = d_small)
save(cell4, file = 'fdcm_cell4.rda') 


# Extraction of the datasets 

for (i in 1:20) {
  sim <- cell4[[i]]  
  file_name <- paste("sim", i, ".txt", sep = '')  
  write.table(sim, file_name)  
}
