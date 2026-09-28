library("rstan")
rstan_options(auto_write = TRUE)     
stopifnot(packageVersion("rstan") >= "2.26")   

n_chains <- 2
options(mc.cores = n_chains)         


target_wd <- "/home/akislyonkova/DCM/fdcm"
if (dir.exists(target_wd) && normalizePath(getwd()) != normalizePath(target_wd)) {
  setwd(target_wd)
}
print(getwd())

respMatrix <- as.matrix(read.table("dark3.txt"))
storage.mode(respMatrix) <- "integer"

Ns <- 5
Nc <- 8
Ni <- ncol(respMatrix)
Np <- nrow(respMatrix)

stopifnot(
  Ni == 27,                                       
  !anyNA(respMatrix),
  all(respMatrix >= 1L & respMatrix <= Ns)         
)

stan_data <- list(Y = respMatrix, Ns = Ns, Np = Np, Ni = Ni, Nc = Nc)


init_fun <- function(chain_id = 1) list(Vc = rep(1 / Nc, Nc))

model <- stan_model("./FDCM_3D_optimized.stan")

start.time <- Sys.time()
fdcm <- sampling(model,
                 data   = stan_data,
                 init   = init_fun,
                 iter   = 6000,        
                 chains = n_chains,
                 seed   = 2026)
end.time <- Sys.time()
print(difftime(end.time, start.time, units = "mins"))

saveRDS(fdcm, file = "fdcm_D3.rds")


check_hmc_diagnostics(fdcm)

FDCM <- as.data.frame(summary(fdcm, pars = c("Vc", "l0", "lam", "tau"))$summary)
bad  <- sum(FDCM$Rhat > 1.05, na.rm = TRUE)
if (bad == 0) {
  print("converged: all Rhat <= 1.05")
} else {
  print(paste(bad, "parameters have Rhat > 1.05 - not converged"))
}
