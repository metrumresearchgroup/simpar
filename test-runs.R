
library(simpar)
library(testthat)

# # simple omega and sigma matrix
theta <- c(1,2,3,4)
covar <- diag(0.1, 4, 4)
omega <- bmat(1, 0, 3)
sigma <- bmat(0.1, 0, 0.3)

simpar(nsim = 1, theta, covar, omega, sigma, 10, 10,
       omega_diag = TRUE, sigma_diag = TRUE)
simpar(nsim = 5, theta, covar, omega, sigma, 10, 10,
       omega_diag = TRUE, sigma_diag = TRUE)
simpar(nsim = 1, theta, covar, omega, sigma, 10, 10,
       omega_diag = TRUE, sigma_diag = TRUE, mrgsolve_style = TRUE)
simpar(nsim = 5, theta, covar, omega, sigma, 10, 10,
       omega_diag = TRUE, sigma_diag = TRUE, mrgsolve_style = TRUE)
#
# complex omega matrix and simple sigma matrix
theta <- c(1,2,3,4)
covar <- diag(0.1, 4, 4)
omega <- list(bmat(1, 0.1, 1, 0.1, 0.1, 1), # 3x3
              bmat(1, 0, 3),
              bmat(2, 0.5, 6))
sigma <- matrix(1)

simpar(n = 1, theta, covar, omega, sigma, c(10,10,10), 10,
       omega_diag = TRUE, sigma_diag = TRUE)
simpar(n = 5, theta, covar, omega, sigma, c(10,10,10), 10,
       omega_diag = TRUE, sigma_diag = TRUE)
simpar(n = 1, theta, covar, omega, sigma, c(10,10,10), 10,
       omega_diag = TRUE, sigma_diag = TRUE, mrgsolve_style = TRUE)
simpar(n = 5, theta, covar, omega, sigma, c(10,10,10), 10,
       omega_diag = TRUE, sigma_diag = TRUE, mrgsolve_style = TRUE)
#
# simple omega matrix and complex sigma matrix
theta <- c(1,2,3,4)
covar <- diag(0.1, 4, 4)
omega <- bmat(1, 0, 3)
sigma <- list(bmat(1, 0.1, 1, 0.1, 0.1, 1), # 3x3
              bmat(1, 0, 3),
              bmat(2, 0.5, 6))

simpar(n = 1, theta, covar, omega, sigma, 10, c(10,10,10),
       omega_diag = TRUE, sigma_diag = TRUE)
simpar(n = 5, theta, covar, omega, sigma, 10, c(10,10,10),
       omega_diag = TRUE, sigma_diag = TRUE)
simpar(n = 1, theta, covar, omega, sigma, 10, c(10,10,10),
       omega_diag = TRUE, sigma_diag = TRUE, mrgsolve_style = TRUE)
simpar(n = 5, theta, covar, omega, sigma, 10, c(10,10,10),
       omega_diag = TRUE, sigma_diag = TRUE, mrgsolve_style = TRUE)

# complex omega and sigma matrix
theta <- c(1,2,3,4)
covar <- diag(0.1, 4, 4)
omega <- list(bmat(2, 0.1, 2, 0.1, 0.1, 2), # 3x3
              bmat(2, 0, 6),
              bmat(4, 0.5, 12))
sigma <- list(bmat(1, 0.1, 1, 0.1, 0.1, 1), # 3x3
              bmat(1, 0, 3),
              bmat(2, 0.5, 6))

simpar(n = 1, theta, covar, omega, sigma, c(10,10,10), c(10,10,10),
       omega_diag = TRUE, sigma_diag = TRUE)
simpar(n = 5, theta, covar, omega, sigma, c(10,10,10), c(10,10,10),
       omega_diag = TRUE, sigma_diag = TRUE)
simpar(n = 1, theta, covar, omega, sigma, c(10,10,10), c(10,10,10),
       omega_diag = TRUE, sigma_diag = TRUE, mrgsolve_style = TRUE)
simpar(n = 5, theta, covar, omega, sigma, c(10,10,10), c(10,10,10),
       omega_diag = TRUE, sigma_diag = TRUE, mrgsolve_style = TRUE)

#' 2) argument name: call style?
#' 2) omega_diag /sigma_diag: change names?
#' Kyle go through the code?
#' 4) Vagnette?
#' 3) Coverage? test how many percentage of code has been tested
#' Merge with PR is the last step
#' 1) Benchmarking? mrgsolve style or not? large matrix vs small matrix? diag==TRUE or not
#' 5) stopifnot to if stop
#' 6) reformat 80 character in width
#' Meeting Sara and Katherine (30 min) after labor days
