# Simulate datasets 

set.seed(2025)
s <- 4

# function to generate data
gendata_rrdm <- function (n_dataset, alpha, item, step, N, i) {
  responses <- list()
  for (n in 1:n_dataset){     # n - number of datasets 
    r <- matrix(NA, N, i)
    for(k in 1:nrow(alpha)) { # k = N 
      for(m in 1:i) {         # i = items in a test
        t1=exp(1*(item[m,1]+item[m,2]*alpha[k,1]) + step[m,1])
        t2=exp(2*(item[m,1]+item[m,2]*alpha[k,1]) + step[m,1] + step[m,2])
        t3=exp(3*(item[m,1]+item[m,2]*alpha[k,1]) + step[m,1] + step[m,2] + step[m,3])
        t4=exp(4*(item[m,1]+item[m,2]*alpha[k,1]) + step[m,1] + step[m,2] + step[m,3] + step[m,4])
        sum=1+t1+t2+t3+t4
        t[m,] = c(1/sum, t1/sum,t2/sum,t3/sum, t4/sum)
        ppp=rmultinom(n=1, size=1, prob=t[m,])  # n = number of random vectors, prob = probabilities sum=1
        r[k,m]=which(ppp == 1, arr.ind=TRUE)[1] # which() function returns the position/index of the value
      }
    }
    responses[[length(responses)+1]] = as.data.frame(r)
  }
  responses
}

###########################################################################################################

### Generating cell 1: large sample, short test

N <- 3000      # sample size
i <- 10        # number of items 
n_id <- 5      # number of items per dimension 

t <- matrix(NA, i, s+1)
#r <- matrix(NA, N, i) 

Q <- matrix(c(rep(c(1,0), n_id),rep(c(0,1), n_id)), i, 2, byrow = T) # i items and 2 attributes
colnames(Q) <- c('A1','A2')
n_attr <- dim(Q)[2]

#define alpha pattern
alpha.patt <- expand.grid(replicate(n_attr, 0:1, simplify = F)) # profile set
AP <- nrow(alpha.patt)                                          # extracts the number of profiles
x <- runif(2^n_attr, min = 0, max = 1)                          # generate 2^A random values between 1 and 0
alpha.prob <- x/sum(x)                                          # normalize this vector so that the sum is equal to 1
ind <- sample(x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ind, ]                                      # simulated pattern ("truth") for all attributes for each person

###  Generating the intercepts, main effects and steps
item_i <- matrix(runif(i, min = -1, max = 1), i, 1, byrow = T) 
item_m <- matrix(runif(i, min = 0.9, max = 2.2), i, 1, byrow = T) 
item <- cbind(item_i, item_m)
item <- as.data.frame(round(item,4))
colnames(item) <- c('I', 'M')

step <- matrix(runif(n_attr * s, min = 0.1, max = 0.5), n_attr, s, byrow = TRUE)
step <- step[rep(1:nrow(step), each = n_id), ]
step <- as.data.frame(round(step,4))
colnames(step) <- c('step1_', 'step2_', 'step3_', 'step4_')

###  Combine and save population parameters 
item_unlist <- as.data.frame(unlist(item, use.names = T))
step_unlist <- as.data.frame(unlist(step, use.names = T))
step_selected <- step_unlist[seq(from = n_id, to = nrow(step_unlist), by = n_id), , drop = FALSE]


colnames(item_unlist) <- 'V1'
colnames(step_selected) <- 'V1'
cell1_param <- rbind(item_unlist, step_selected)

write.table(cell1_param, file = 'RRDM_cell1_param.txt') # save cell 1 params

###  Genarate and save datasets for cell 1 
cell1 <- gendata_rrdm(n_dataset = 25, alpha = alpha, item = item,  step = step, N = N, i = i) # generate data
save(cell1, file = 'rrdm_cell1.rda') # saves generated cell 1 

##########################################################################################################

### Generating cell 2: short test, small sample 

N <- 1500      
i <- 10        
n_id <- 5     

t <- matrix(NA, i, s+1)
r <- matrix(NA, N, i) 

Q <- matrix(c(rep(c(1,0), n_id),rep(c(0,1), n_id)), i, 2, byrow = T) # 20 items and 2 attributes
colnames(Q) <- c('A1','A2')
n_attr <- dim(Q)[2]

#define alpha pattern
alpha.patt <- expand.grid(replicate(n_attr, 0:1, simplify = F)) 
AP <- nrow(alpha.patt)                                          
x <- runif(2^n_attr, min = 0, max = 1)                          
alpha.prob <- x/sum(x)                                          
ind <- sample(x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ind, ]                                      

###  Generating the intercepts, main effects and steps
item_i <- matrix(runif(i, min = -2.2, max = -0.9), i, 1, byrow = T) 
item_m <- matrix(runif(i, min = 0.9, max = 2.2), i, 1, byrow = T) 
item <- cbind(item_i, item_m)
item <- as.data.frame(round(item,4))
colnames(item) <- c('I', 'M')

step <- matrix(runif(n_attr * s, min = 0, max = 0.5), n_attr, s, byrow = TRUE)
step <- step[rep(1:nrow(step), each = n_id), ]
step <- as.data.frame(round(step,4))
colnames(step) <- c('step1_', 'step2_', 'step3_', 'step4_')

###  Combine and save population parameters 
item_unlist <- as.data.frame(unlist(item, use.names = T))
step_unlist <- as.data.frame(unlist(step, use.names = T))
step_selected <- step_unlist[seq(from = n_id, to = nrow(step_unlist), by = n_id), , drop = FALSE]


colnames(item_unlist) <- 'V1'
colnames(step_selected) <- 'V1'
cell2_param <- rbind(item_unlist, step_selected)

write.table(cell2_param, file = 'RRDM_cell2_param.txt') 

###  Genarate and save datasets for cell 2 
cell2 <- gendata_rrdm(n_dataset = 25, alpha = alpha, item = item,  step = step) 
save(cell2, file = 'rrdm_cell2.rda')  



##########################################################################################################

### Generating cell 3: long test, large sample 

N <- 3000      
i <- 20        
n_id <- 10    

t <- matrix(NA, i, s+1)
r <- matrix(NA, N, i) 

Q <- matrix(c(rep(c(1,0), n_id),rep(c(0,1), n_id)), i, 2, byrow = T) # i items and 2 attributes
colnames(Q) <- c('A1','A2')
n_attr <- dim(Q)[2]

#define alpha pattern
alpha.patt <- expand.grid(replicate(n_attr, 0:1, simplify = F)) 
AP <- nrow(alpha.patt)                                          
x <- runif(2^n_attr, min = 0, max = 1)                          
alpha.prob <- x/sum(x)                                          
ind <- sample(x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ind, ]                                      

###  Generating the intercepts, main effects and steps
item_i <- matrix(runif(i, min = -2.2, max = -0.9), i, 1, byrow = T) 
item_m <- matrix(runif(i, min = 0.9, max = 2.2), i, 1, byrow = T) 
item <- cbind(item_i, item_m)
item <- as.data.frame(round(item,4))
colnames(item) <- c('I', 'M')

step <- matrix(runif(n_attr * s, min = 0, max = 0.5), n_attr, s, byrow = TRUE)
step <- step[rep(1:nrow(step), each = n_id), ]
step <- as.data.frame(round(step,4))
colnames(step) <- c('step1_', 'step2_', 'step3_', 'step4_')

###  Combine and save population parameters 
item_unlist <- as.data.frame(unlist(item, use.names = T))
step_unlist <- as.data.frame(unlist(step, use.names = T))
step_selected <- step_unlist[seq(from = n_id, to = nrow(step_unlist), by = n_id), , drop = FALSE]


colnames(item_unlist) <- 'V1'
colnames(step_selected) <- 'V1'
cell3_param <- rbind(item_unlist, step_selected)

write.table(cell3_param, file = 'RRDM_cell3_param.txt') 

###  Genarate and save datasets for cell 2 
cell3 <- gendata_rrdm(n_dataset = 25, alpha = alpha, item = item,  step = step) 
save(cell3, file = 'rrdm_cell3.rda') 

##########################################################################################################

### Generating cell 4: long test, small sample 

N <- 1500      
i <- 20        
n_id <- 10     

t <- matrix(NA, i, s+1)
r <- matrix(NA, N, i) 

Q <- matrix(c(rep(c(1,0), n_id),rep(c(0,1), n_id)), i, 2, byrow = T) # 40 items and 2 attributes
colnames(Q) <- c('A1','A2')
n_attr <- dim(Q)[2]

#define alpha pattern
alpha.patt <- expand.grid(replicate(n_attr, 0:1, simplify = F)) 
AP <- nrow(alpha.patt)                                          
x <- runif(2^n_attr, min = 0, max = 1)                          
alpha.prob <- x/sum(x)                                          
ind <- sample(x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ind, ]                                      

###  Generating the intercepts, main effects and steps
item_i <- matrix(runif(i, min = -2.2, max = -0.9), i, 1, byrow = T) 
item_m <- matrix(runif(i, min = 0.9, max = 2.2), i, 1, byrow = T) 
item <- cbind(item_i, item_m)
item <- as.data.frame(round(item,4))
colnames(item) <- c('I', 'M')

step <- matrix(runif(n_attr * s, min = 0, max = 0.5), n_attr, s, byrow = TRUE)
step <- step[rep(1:nrow(step), each = n_id), ]
step <- as.data.frame(round(step,4))
colnames(step) <- c('step1_', 'step2_', 'step3_', 'step4_')

###  Combine and save population parameters 
item_unlist <- as.data.frame(unlist(item, use.names = T))
step_unlist <- as.data.frame(unlist(step, use.names = T))
step_selected <- step_unlist[seq(from = n_id, to = nrow(step_unlist), by = n_id), , drop = FALSE]


colnames(item_unlist) <- 'V1'
colnames(step_selected) <- 'V1'
cell4_param <- rbind(item_unlist, step_selected)

write.table(cell4_param, file = 'RRDM_cell4_param.txt') 

###  Genarate and save datasets for cell 2 
cell4 <- gendata_rrdm(n_dataset = 25, alpha = alpha, item = item,  step = step) 
save(cell4, file = 'rrdm_cell4.rda')

# Extraction of the datasets 

for (d_idx in 1:25) {
  sim <- cell1[[d_idx]]  
  file_name <- paste("sim", d_idx, ".txt", sep = '')  
  write.table(sim, file_name)  
}
