# Simulate datasets 

### With existing parameters 

# library(rio)
# steps <- import("step.xlsx")
# item <- import("item.xlsx")
# steps <- steps[rep(seq_len(nrow(steps)), each = 10), ]
# step <- round(steps,4) # rounds the step parameters to the 4th decimal point
# item <- round(item,4)

### With generated parameters 

t <- matrix(NA,40,5) # empty T matrix 40 questions and 4 profiles (item by profile matrix)
r <- matrix(NA,901,40) # empty response matrix, 40 questions, 901 people
responses <- list() # creates an empty list for holding responses 


set.seed(2024)
Q=matrix(c(rep(c(1,0,0,0),10),rep(c(0,1,0,0),10),
           rep(c(0,0,1,0),10),rep(c(0,0,0,1),10)), 40, 4, byrow = T) # 40 questions and 4 attributes
colnames(Q) <- c("A1","A2", "A3", "A4")
n_attr<-dim(Q)[2]
alpha.patt<-(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) # profile set
AP <- nrow(alpha.patt) # extracts the number of profiles
x <- runif(16, min = 0, max = 1) # generate 16 random values between 1 and 0
alpha.prob <- x/sum(x) # normalize this vector so that the sum is equal to 1
N <- 901 # number of people
ind <- sample(x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ind, ] # simulated pattern ("truth") for all attributes for each person



step <- matrix(runif(16, min = -3, max = 3), 4, 4, byrow = T) # generates 16 random step values from a uniform distribution
step <- round(step[rep(seq_len(nrow(step)), each = 10), ],4) # rounds the step parameters to the 4th decimal point 
                                                             # and repeats each step within dimension
item <- matrix(runif(40, min = -3, max = 3), 40, 2, byrow = T) # generates 40 random intercepts and main effects
item <- round(item,4)


# the loops generate responses for each person on each question based on their alpha pattern 
for(k in 1:nrow(alpha)) {  # k = N 
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



export(r,'dataset40.xlsx')


# responses[[length(responses)+1]]=as.data.frame(r)
# save(responses, file = "simresponses.rda")
