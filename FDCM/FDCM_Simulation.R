# Simulate datasets 

set.seed(2025)
N <- 700 
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
n_attr<-dim(Q)[2]

#define alpha pattern
alpha.patt <- expand.grid(replicate(n_attr, 0:1, simplify = F)) # profile set
AP <- nrow(alpha.patt) # extracts the number of profiles
x <- runif(2^n_attr, min = 0, max = 1) # generate 2^A random values between 1 and 0
alpha.prob <- x/sum(x) # normalize this vector so that the sum is equal to 1
ind <- sample(x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ind, ] # simulated pattern ("truth") for all attributes for each person

###  Generating the intercepts parameters
item_i <- matrix(runif(i, min = -1, max = 1), i, 1, byrow = T) 


###  Generating the dispersion parameters
d <- matrix(runif(i, min = 0, max = 1), i, 1, byrow = T) 


### Generating the main effects
### Large: min = 0.9, max = 3

item_m <- matrix(runif(i, min = 0.9, max = 3), i, 1, byrow = T) 
item <- cbind(item_i, item_m)
item <- as.data.frame(round(item,4))
colnames(item) <- c('I', 'M')
#write.table(item, file = 'item_cell1.txt')


# Saving the parameters used for generating the data
item_unlist <- as.data.frame(unlist(item, use.names = T))
d_param <- as.data.frame(unlist(d, use.names = T))
d_unlist <- as.data.frame(unlist(d_param, use.names = T))
colnames(item_unlist) <- 'V1'
colnames(d_unlist) <- 'V1'
cell1_param <- rbind(item_unlist, d_unlist)
write.table(cell1_param, file = 'cell1_param.txt')

cell1 <- gendata_fdcm(n_dataset = 1, alpha = alpha, item = item,  d = d)


### Generating small  main effects
item_m <- matrix(runif(i, min = -1, max = 1), i, 1, byrow = T)
item <- cbind(item_i, item_m)
item <- round(item,4)
colnames(item) <- c('I', 'M')

cell2 <- gendata_fdcm(n_dataset = 1, alpha = alpha, item = item, d = d)

### Saving the results 
save(cell1, file = 'fdcm_cell1.rda') # saves generated datasets 


 

### With existing parameters 

# library(rio)
# d <- import("d.xlsx")
# item <- import("item.xlsx")
# item <- round(item,4)


