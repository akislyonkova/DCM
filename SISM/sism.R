library(GDINA)

# --- 1. Define Q-Matrix ---
# Cols 1-2: Skills (S1, S2)
# Col 3: Bug (B1) - MUST be the last column(s)
Q <- matrix(c(
  1, 0, 0,  # Item 1: Requires S1
  0, 1, 0,  # Item 2: Requires S2
  1, 1, 0,  # Item 3: Requires S1 + S2
  0, 0, 1,  # Item 4: Pure Bug 1 Item
  1, 0, 1,  # Item 5: Requires S1, Bug 1 interferes
  1, 1, 1   # Item 6: Requires S1 + S2, Bug 1 interferes
), ncol = 3, byrow = TRUE)

colnames(Q) <- c("Skill1", "Skill2", "Bug1")

# --- 2. Manually Simulate SISM Data (Bypassing simGDINA errors) ---
set.seed(123)
N <- 1000
J <- nrow(Q)
K <- ncol(Q)

# A. Generate Random Attributes for N Examinees
# (0 = No Skill/Bug, 1 = Has Skill/Bug)
# We assume uniform distribution for simplicity
profiles <- attributepattern(K)
true_alpha_indices <- sample(1:nrow(profiles), N, replace = TRUE)
true_alpha <- profiles[true_alpha_indices, ]

# B. Define SISM Parameters
g <- 0.1 # Guessing (prob of success if you lack skills OR have bug)
s <- 0.1 # Slip (prob of failure if you have skills AND no bug)

# C. Generate Responses
dat <- matrix(0, nrow = N, ncol = J)

for (i in 1:N) {
  for (j in 1:J) {
    # Get examinee's alpha and item's Q-vector
    alpha_i <- true_alpha[i, ]
    q_j <- Q[j, ]
    
    # Identify indices of required skills and bugs
    # We assume Bug is the LAST column (index 3)
    idx_skills <- which(q_j[1:2] == 1)
    idx_bugs   <- which(q_j[3] == 1) + 2 # Offset by 2 skills
    
    # --- SISM LOGIC ---
    # 1. Check Skills (Conjunctive / DINA logic)
    # Must have ALL required skills
    has_skills <- all(alpha_i[idx_skills] == 1)
    
    # 2. Check Bugs (Disjunctive / DINO logic)
    # Must NOT have ANY required bugs
    if (length(idx_bugs) > 0) {
      has_bugs <- any(alpha_i[idx_bugs] == 1)
    } else {
      has_bugs <- FALSE
    }
    
    # 3. Determine Probability
    # Success only if: Has Skills AND No Bugs
    if (has_skills && !has_bugs) {
      prob <- 1 - s
    } else {
      prob <- g
    }
    
    # 4. Simulate Response (Bernoulli trial)
    dat[i, j] <- rbinom(1, 1, prob)
  }
}

start.time <- Sys.time()
# --- 3. Estimate SISM ---
# Now we run the GDINA function on our manually created data
# Note: 'bugs' argument requires the COLUMN NAMES of the bugs
fit_sism <- GDINA(dat = dat, 
                  Q = Q, 
                  model = "SISM", 
                  no.bugs = 1) # Must match Q colname
end.time <- Sys.time()
time.taken <- end.time - start.time
# --- 4. Review Results ---
summary(fit_sism)

# Check Item Parameters
# For SISM: 
# P(0) = guessing probability (approx 0.1)
# P(1) = 1 - slip probability (approx 0.9)
coef(fit_sism)

# 
# To visualize the prevalence of Skills vs Bugs:
plot(fit_sism, what = "prob")

##############################################################################################################
# example in GDINA package 

##############################################################
# Example 15
# reparameterized SISM model (Kuo, Chen, & de la Torre, 2018)
# see GDINA function for more details
###############################################################
# The Q-matrix used in Kuo, et al (2018)
# The first four columns are for Attributes 1-4
# The last three columns are for Bugs 1-3
Q <- matrix(c(1,0,0,0,0,0,0,
              0,1,0,0,0,0,0,
              0,0,1,0,0,0,0,
              0,0,0,1,0,0,0,
              0,0,0,0,1,0,0,
              0,0,0,0,0,1,0,
              0,0,0,0,0,0,1,
              1,0,0,0,1,0,0,
              0,1,0,0,1,0,0,
              0,0,1,0,0,0,1,
              0,0,0,1,0,1,0,
              1,1,0,0,1,0,0,
              1,0,1,0,0,0,1,
              1,0,0,1,0,0,1,
              0,1,1,0,0,0,1,
              0,1,0,1,0,1,1,
              0,0,1,1,0,1,1,
              1,0,1,0,1,1,0,
              1,1,0,1,1,1,0,
              0,1,1,1,1,1,0),ncol = 7,byrow = TRUE)
J <- nrow(Q)
N <- 500
gs <- data.frame(guess=rep(0.1,J),slip=rep(0.1,J))
sim <- simGDINA(N,Q,gs.parm = gs,model = "SISM",no.bugs=3)
# True item success probabilities
extract(sim,what = "catprob.parm")
# True delta parameters
extract(sim,what = "delta.parm")
# simulated data
extract(sim,what = "dat")
# simulated attributes
extract(sim,what = "attribute")
## End(Not run)
