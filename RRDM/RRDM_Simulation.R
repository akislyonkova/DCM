# Simulate datasets 

set.seed(2024)
N <- 700 # number of people
i <- 30  # number of items


# function to generate data
responses <- list() # creates an empty list for holding responses 
gendata_rrdm <- function (n_dataset, step, item, alpha, Q) {
  for (n in 1:n_dataset){     # n - number of datasets 
    for(k in 1:nrow(alpha)) { # k = N 
      for(i in 1:10) {        # i - item, questions 1 through 10 (1st attribute) 
        t1=exp(1*(item[i,1]+item[i,2]*alpha[k,1]) + step[i,1])
        t2=exp(2*(item[i,1]+item[i,2]*alpha[k,1]) + step[i,1] + step[i,2])
        t3=exp(3*(item[i,1]+item[i,2]*alpha[k,1]) + step[i,1] + step[i,2] + step[i,3])
        t4=exp(4*(item[i,1]+item[i,2]*alpha[k,1]) + step[i,1] + step[i,2] + step[i,3] + step[i,4])
        sum=1+t1+t2+t3+t4
        t[i,] = c(1/sum, t1/sum,t2/sum,t3/sum, t4/sum)
        ppp=rmultinom(n=1, size=1, prob=t[i,]) # n- number of random vectors, prob - probabilities sum=1
        r[k,i]=which(ppp == 1, arr.ind=TRUE)[1] #which() function returns the position/index of the value
      }
      for(i in 11:20) { # questions 11 through 20 (2nd attribute)
        t1=exp(1*(item[i,1]+item[i,2]*alpha[k,2]) + step[i,1])
        t2=exp(2*(item[i,1]+item[i,2]*alpha[k,2]) + step[i,1] + step[i,2])
        t3=exp(3*(item[i,1]+item[i,2]*alpha[k,2]) + step[i,1] + step[i,2] + step[i,3])
        t4=exp(4*(item[i,1]+item[i,2]*alpha[k,2]) + step[i,1] + step[i,2] + step[i,3] + step[i,4])
        sum=1+t1+t2+t3+t4
        t[i,]=c(1/sum,t1/sum,t2/sum,t3/sum, t4/sum)
        ppp=rmultinom(n=1, size=1, prob=t[i,]) # n- number of random vectors, prob - probabilities sum=1
        r[k,i]=which(ppp == 1, arr.ind=TRUE)[1] #which() function returns the position/index of the value
      }
      for(i in 21:30) { # questions 21 through 30 (3rd attribute)
        t1=exp(1*(item[i,1]+item[i,2]*alpha[k,3]) + step[i,1])
        t2=exp(2*(item[i,1]+item[i,2]*alpha[k,3]) + step[i,1] + step[i,2])
        t3=exp(3*(item[i,1]+item[i,2]*alpha[k,3]) + step[i,1] + step[i,2] + step[i,3])
        t4=exp(4*(item[i,1]+item[i,2]*alpha[k,3]) + step[i,1] + step[i,2] + step[i,3] + step[i,4])
        sum=1+t1+t2+t3+t4
        t[i,]=c(1/sum,t1/sum,t2/sum,t3/sum, t4/sum)
        ppp=rmultinom(n=1, size=1, prob=t[i,])
        r[k,i]=which(ppp == 1, arr.ind=TRUE)[1]
      }
      for(i in 31:40) { # questions 31 through 40 (4th attribute)
        t1=exp(1*(item[i,1]+item[i,2]*alpha[k,4]) + step[i,1])
        t2=exp(2*(item[i,1]+item[i,2]*alpha[k,4]) + step[i,1] + step[i,2])
        t3=exp(3*(item[i,1]+item[i,2]*alpha[k,4]) + step[i,1] + step[i,2] + step[i,3])
        t4=exp(4*(item[i,1]+item[i,2]*alpha[k,4]) + step[i,1] + step[i,2] + step[i,3] + step[i,4])
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

t <- matrix(NA, i, 5) # empty t matrix 
r <- matrix(NA, N, i) # empty response matrix
#define Q
Q=matrix(c(rep(c(1,0,0),10),rep(c(0,1,0),10),
           rep(c(0,0,1),10)), 30, 3, byrow = T) # 30 questions and 3 attributes
colnames(Q) <- c('A1','A2', 'A3')
n_attr<-dim(Q)[2]

#define alpha pattern
alpha.patt<-(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) # profile set
AP <- nrow(alpha.patt) # extracts the number of profiles
x <- runif(2^n_attr, min = 0, max = 1) # generate 8 random values between 1 and 0
alpha.prob <- x/sum(x) # normalize this vector so that the sum is equal to 1
ind <- sample(x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ind, ] # simulated pattern ("truth") for all attributes for each person

### With existing parameters 

# library(rio)
# steps <- import("step.xlsx")
# item <- import("item.xlsx")
# step <- round(steps[rep(seq_len(nrow(steps)), each = 10), ], 4)
# item <- round(item,4)


###  With generated parameters 

step <- matrix(runif(2^n_attr, min = -1, max = 1), 4, n_attr, byrow = T) # generates 16 random step values from a uniform distribution
step <- round(step[rep(seq_len(nrow(step)), each = 10), ],4) # rounds to the 4 decimal point, repeats each step within a dimension 
item_i <- matrix(runif(i, min = -1, max = 1), i, 1, byrow = T) # generates 40 random intercepts 

### With large main effects

item_m <- matrix(runif(i, min = -3, max = 3), i, 1, byrow = T) # generates 40 random  main effects
item <- cbind(item_i, item_m)
item <- round(item,4)

cell1 <- gendata_rrdm(n_dataset = 20, alpha = alpha, item = item, step = step)


### With small  main effects
item_m <- matrix(runif(i, min = -1, max = 1), i, 1, byrow = T)
item <- cbind(item_i, item_m)
item <- round(item,4)

cell2 <- gendata_rrdm(n_dataset = 20, alpha = alpha, item = item, step = step)

### Saving the results 
save(cell1, file = 'rrdm_cell1.rda') # saves generated datasets 
load(file='rrdm_cell1.rda') # loads the responses file 
simdata_9 <- cell1[[9]] # extracts a single dataset  
