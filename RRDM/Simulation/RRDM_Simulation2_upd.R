# Simulate datasets 

set.seed(2025)

# function to generate data

gendata_rrdm <- function (n_dataset, alpha, item, step, N, i) {
  responses <- list()
  s <- ncol(step) 
  item_attr_map <- apply(Q, 1, function(x) which(x == 1))
  
  for (n in 1:n_dataset){
    r <- matrix(NA, N, i)
    
    for(m in 1:i) { 
      target_attr_col <- item_attr_map[m]
      
      # Prepare parameters
      b <- item[m,1] 
      a <- item[m,2] 
      
      # Vectorized Kernel: This is a vector of length N
      kernel <- b + a * alpha[, target_attr_col]
      
      s1 <- step[m,1]
      s2 <- step[m,2]
      s3 <- step[m,3]
      s4 <- step[m,4]
      
      thr1 <- exp(1 * kernel + s1)
      thr2 <- exp(2 * kernel + s1 + s2)
      thr3 <- exp(3 * kernel + s1 + s2 + s3)
      thr4 <- exp(4 * kernel + s1 + s2 + s3 + s4)
      
      summ <- 1 + thr1 + thr2 + thr3 + thr4
      
      # Probabilities matrix (N x 5)
      probs <- cbind(1/summ, thr1/summ, thr2/summ, thr3/summ, thr4/summ)
      
      # Generate responses row by row using the specific probabilities for each person
      # We use apply() to draw a multinomial for each row (person)
      # This returns a vector of categories (1-5)
      draws <- apply(probs, 1, function(x) which(rmultinom(1, 1, x) == 1))
      
      r[, m] <- draws
    }
    responses[[length(responses)+1]] <- as.data.frame(r)
  }
  responses
}

###########################################################################################################

### Generating cell 1: large sample, short test

N <- 3000      # sample size
i <- 10        # number of items 
n_id <- 5      # number of items per dimension 
s <- 4 

thr <- matrix(NA, i, s+1)

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

step <- matrix(runif(n_attr * s, min = -0.5, max = 0), n_attr, s, byrow = TRUE)
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


# Extraction of the datasets 

for (d_idx in 1:25) {
  sim <- cell1[[d_idx]]  
  file_name <- paste("sim", d_idx, ".txt", sep = '')  
  write.table(sim, file_name)  
}
