library(GDINA)

Q <- matrix(c(1,0,0,0,0,0,0, 0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0, 
                   0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1, 1,0,0,0,1,0,0,
                   0,1,0,0,1,0,0, 0,0,1,0,0,0,1, 0,0,0,1,0,1,0, 1,1,0,0,1,0,0,
                   1,0,1,0,0,0,1, 1,0,0,1,0,0,1, 0,1,1,0,0,0,1, 0,1,0,1,0,1,1,
                   0,0,1,1,0,1,1, 1,0,1,0,1,1,0, 1,1,0,1,1,1,0, 0,1,1,1,1,1,0),
                 ncol = 7, byrow = TRUE)
J <- nrow(Q)
N <- 1000
gs <- data.frame(guess=rep(0.1,J),slip=rep(0.1,J))
sim <- simGDINA(N,Q,gs.parm = gs,model = "SISM",no.bugs=3)
dat <- extract(sim,"dat")
est <- GDINA(dat=dat,Q=Q,model="SISM",no.bugs=3)
coef(est,"delta")

est2 <- GDINA(dat=dat,Q=Q,model="GDINA",link="logit", solver = "slsqp")


AIC(est, est2)
BIC(est, est2)
