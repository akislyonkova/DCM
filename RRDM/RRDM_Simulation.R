# Simulate dataset

set.seed(666)
Q=matrix(c(rep(c(1,0,0,0),10),rep(c(0,1,0,0),10),rep(c(0,0,1,0),10),rep(c(0,0,0,1),10)),40,4, byrow=T) # 40 questions and 4 attributes 
colnames(Q) <- c("A1","A2", "A3", "A4")
n_attr<-dim(Q)[2]
alpha.patt<-(expand.grid(replicate(n_attr, 0:1, simplify = FALSE))) # profile set 
AP <- nrow(alpha.patt) # extracts the number of profiles 
x <- runif(16, min = 0, max = 1) # generate 16 random values between 1 and 0
x1 <- x/sum(x) # normalize this vector so that the sum is equal to 1 
alpha.prob <- x1 # random values between 0 and 1 that give 1 in sum
N <- 901 # number of people 
ind <- sample(x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ind,] # simulated pattern ("truth") for all attributes for each person 

# a document with parameters for an RRDM 
#library("rstan")
#RRDM<-summary(estimated_model)$summary
library(rio)
steps <- import("C:/Users/User/Desktop/param.xlsx")
main<-rnorm(80)
#main <- import("C:/Users/User/Desktop/main.xlsx")

main <- matrix(main,40,2) # matrix with 2 vectors (main effects and intercepts) for 40 questions
colnames(main) <- c("l0","l1") # names each step 
steps <- (round(steps,4)) # rounds the step parameters to the 4th decimal point 
main<-(round(main,4))
t <- matrix(NA,40,5) # empty T matrix 40 questions and 4 profiles (item by profile matrix)
r <- matrix(NA,901,40) # empty response matrix, 40 questions, 901 people 
responses <- list() # creates an empty list for holding responses 

# this is the main part 
# the loops generate responses for each person on each question based on the alpha patterns 

for(k in 1:nrow(alpha)) { # k - person 
  for(i in 1:10) { # i - item, questions 1 through 10 (1st attribute) 
    t1=exp(1*(main[i,1]+main[i,2]*alpha[k,1]) + step[i,1])
    t2=exp(2*(main[i,1]+main[i,2]*alpha[k,1]) + step[i,1] + step[i,2])
    t3=exp(3*(main[i,1]+main[i,2]*alpha[k,1]) + step[i,1] + step[i,2] + step[i,3])
    t4=exp(4*(main[i,1]+main[i,2]*alpha[k,1]) + step[i,1] + step[i,2] + step[i,3] + step[i,4])
    sum=1+t1+t2+t3+t4
    t[i,]=c(1, t1/sum,t2/sum,t3/sum, t4/sum) 
    ppp=rmultinom(n=1, size=1, prob=t[i,]) # n- number of random vectors, prob - probabilities sum=1
    r[k,i]=which(ppp == 1, arr.ind=TRUE)[1] #which() function returns the position/index of the value
  }
  for(i in 11:20) { # questions 11 through 20 (2nd attribute) 
    t1=exp(1(main[i,1]+main[i,2]*alpha[k,2]) + step[i,1])
    t2=exp(2(main[i,1]+main[i,2]*alpha[k,2]) + step[i,1] + step[i,2])
    t3=exp(3(main[i,1]+main[i,2]*alpha[k,2]) + step[i,1] + step[i,2] + step[i,3])
    t4=exp(4(main[i,1]+main[i,2]*alpha[k,2]) + step[i,1] + step[i,2] + step[i,3] + step[i,4])
    sum=1+t1+t2+t3+t4
    t[i,]=c(1,t1/sum,t2/sum,t3/sum, t4/sum) 
    ppp=rmultinom(n=1, size=1, prob=t[i,]) # n- number of random vectors, prob - probabilities sum=1
    r[k,i]=which(ppp == 1, arr.ind=TRUE)[1] #which() function returns the position/index of the value
  }
  for(i in 21:30) { # questions 21 through 30 (3rd attribute) 
    t1=exp(1*(main[i,1]+main[i,2]*alpha[k,3]) + step[i,1])
    t2=exp(2*(main[i,1]+main[i,2]*alpha[k,3]) + step[i,1] + step[i,2])
    t3=exp(3*(main[i,1]+main[i,2]*alpha[k,3]) + step[i,1] + step[i,2] + step[i,3])
    t4=exp(4*(main[i,1]+main[i,2]*alpha[k,3]) + step[i,1] + step[i,2] + step[i,3] + step[i,4])
    sum=1+t1+t2+t3+t4
    t[i,]=c(1,t1/sum,t2/sum,t3/sum, t4/sum) 
    ppp=rmultinom(n=1, size=1, prob=t[i,]) 
    r[k,i]=which(ppp == 1, arr.ind=TRUE)[1]
  }
  for(i in 31:40) { # questions 31 through 40 (4th attribute) 
    t1=exp(1*(main[i,1]+main[i,2]*alpha[k,4]) + step[i,1])
    t2=exp(2*(main[i,1]+main[i,2]*alpha[k,4]) + step[i,1] + step[i,2])
    t3=exp(3*(main[i,1]+main[i,2]*alpha[k,4]) + step[i,1] + step[i,2] + step[i,3])
    t4=exp(4*(main[i,1]+main[i,2]*alpha[k,4]) + step[i,1] + step[i,2] + step[i,3] + step[i,4])
    sum=1+t1+t2+t3+t4
    t[i,]=c(1,t1/sum,t2/sum,t3/sum, t4/sum) 
    ppp=rmultinom(n=1, size=1, prob=t[i,]) 
    r[k,i]=which(ppp == 1, arr.ind=TRUE)[1]
  }
}
responses[[length(responses)+1]]=as.data.frame(r)

save(responses, file = "simresponses.rda")

