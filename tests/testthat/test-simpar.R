library(simpar)
library(testthat)

context("test-simpar")

omega <- bmat(1, 0, 3)
sigma <- matrix(1)
theta <- c(1,2,3)
covar <- diag(0.1, 3, 3)

test_that("return value as expected", {
  pars <- simpar(100, theta = theta, covar = covar, omega, sigma, 10, 10)
  expect_is(pars, "matrix")
  expect_equal(nrow(pars), 100)
  expect_equal(ncol(pars), (3 + 3 + 1))
  expect_equal(
    colnames(pars),
    c(paste0("TH.", c(1,2,3)), "OM1.1", "OM2.1", "OM2.2", "SG1.1"),
  )
})

test_that("theta return", {
  set.seed(87654)
  covar <- bmat(1, 0, 2, 0, 0, 3)
  pars <- simpar(500, theta = theta, covar = covar, omega, sigma, 10, 10)
  pars <- as.data.frame(pars)
  m1 <- mean(pars$TH.1)
  expect_equal(round(m1), 1)
  m2 <- mean(pars$TH.2)
  expect_equal(round(m2), 2)
  m3 <- mean(pars$TH.3)
  expect_equal(round(m3), 3)
  v1 <- var(pars$TH.1)
  expect_equal(round(v1), 1)
  v2 <- var(pars$TH.2)
  expect_equal(round(v2), 2)
  v3 <- var(pars$TH.3)
  expect_equal(round(v3), 3)
})

test_that("omega return", {
  set.seed(87654)
  sigma <- bmat(1, 0.5, 2)
  pars <- simpar(500, theta = theta[1], covar = covar[1,1], omega, sigma, 100, 40)
  pars <- as.data.frame(pars)
  m1 <- mean(pars$OM1.1)
  expect_equal(round(m1), 1)
  m2 <- mean(pars$OM2.2)
  expect_equal(round(m2), 3)
  pars2 <- simpar(500, theta = theta[1], covar = covar[1,1], omega, sigma, 50, 40)
  pars2 <- as.data.frame(pars2)
  v1 <- var(pars$OM1.1)
  v2 <- var(pars2$OM1.1)
  expect_true(v2 / v1 > 2)
})

test_that("sims are reproducible", {
  set.seed(123)
  pars1 <- simpar(100, theta = theta, covar = covar, omega, sigma, 10, 10)
  set.seed(123)
  pars2 <- simpar(100, theta = theta, covar = covar, omega, sigma, 10, 10)
  expect_identical(pars1, pars2)
})

test_that("posmat returns a matrix with det greater than zero", {
  x <- matrix(1, nrow = 3, ncol = 3)
  ans <- simpar:::posmat(x)
  expect_equal(nrow(ans), 3)
  expect_equal(ncol(ans), 3)
  expect_identical(diag(ans), c(1,1,1))
  expect_true(det(x) <= 0)
  expect_true(det(ans) > 0)
})

test_that("omega and sigma must be symmetric", {
  set.seed(112)
  omega2 <- matrix(rnorm(4), ncol = 2, nrow = 2)
  expect_error(
    simpar(100, theta = theta, covar = covar, omega2, sigma, 10, 10),
    regexp = "matrix is not symmetric"
  )
  sigma2 <- matrix(rnorm(4), ncol = 2, nrow = 2)
  expect_error(
    simpar(100, theta = theta, covar = covar, omega, sigma2, 10, 10),
    regexp = "matrix is not symmetric"
  )
})

test_that("omega and sigma must be positive definite", {
  set.seed(112)
  omega2 <- bmat(1,100,1)
  expect_error(
    simpar(100, theta = theta, covar = covar, omega2, sigma, 10, 10),
    regexp = "not all omega blocks are positive-definite"
  )
  sigma2 <- omega2
  expect_error(
    simpar(100, theta = theta, covar = covar, omega, sigma2, 10, 10),
    regexp = "not all sigma blocks are positive-definite"
  )
})

test_that("simulated parameters respect bounds", {
  bounds <- rep(0, 3 + 3 + 1)
  omega <- bmat(1, 0.1, 3)
  pars1 <- simpar(100, theta = theta, covar = covar, omega, sigma, 10, 10)
  set.seed(456)
  expect_warning(
    pars2 <- simpar(100, theta = theta, covar = covar, omega, sigma, 10, 10, min = bounds),
    regexp = "rows dropped"
  )
  expect_equal(nrow(pars1), 100)
  expect_true(nrow(pars2) < 100)
  mn2 <- sapply(pars2, min)
  expect_true(all(mn2 > 0))
  mn1 <- sapply(pars1, min)
  expect_false(all(mn1 > 0))
  set.seed(456)
  expect_warning(
    pars3 <- simpar(100, theta = theta, covar = covar, omega, sigma, 10, 10, min = 0),
    regexp = "rows dropped"
  )
  expect_identical(pars2, pars3)
})

test_that("outputs match metrumrg implementation", {
  skip_if_not_installed("metrumrg")
  theta <- c(1,2,3,4,5)/10
  covar <- diag(0.1, 5)/seq(1,5)

  # expect same behavior when diag matrix
  omega1 <- diag(c(1,2,3,4))
  sigma1 <- diag(c(10,100))

  # expect same behavior when non-diag matrix
  omega2 <- bmat(1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 1)
  sigma2 <- bmat(10, 0.1, 100)

  set.seed(12345)
  a <- simpar(5, theta, covar, omega1, sigma1)
  set.seed(12345)
  b <- metrumrg::simpar(5, theta, covar, omega1, sigma1)
  expect_identical(a,b)

  set.seed(12345)
  a <- simpar(5, theta, covar, omega2, sigma2)
  set.seed(12345)
  b <- metrumrg::simpar(5, theta, covar, omega2, sigma2)
  expect_identical(a,b)
})

test_that("minimum degrees of freedom is nrow", {
  theta <- c(1,2,3,4,5)/10
  covar <- diag(0.1, 5)/seq(1,5)
  omega <- diag(c(1,2,3,4))
  sigma <- diag(c(10,100))
  expect_type(simpar(5, theta, covar, omega, sigma),"double")
  expect_type(simpar(5, theta, covar, omega, sigma, odf = 4), "double")
  expect_error(
    simpar(5, theta, covar, omega, sigma, odf = 3),
    regexp = "less than number of rows"
  )
})

# simblock
test_that("simblock: zeros are in diagonal only matrix", {
  cov <- matrix(data = c(1,0,0,1), nrow = 2, ncol = 2)

  ans <- simpar:::simblock(n = 3, df = 100, cov = cov, diagonal = TRUE)
  expect_true(all(ans[, 2] == 0))
})

test_that("simblock: simulate non-diagonal matrix", {
  cov <- matrix(data = c(1,0.1,0.1,1), nrow = 2, ncol = 2)

  ans <- simpar:::simblock(n = 3, df = 100, cov = cov, diagonal = TRUE)
  expect_true(all(ans[, 2] != 0))
})

# sblock
test_that("sblock: zeros are in diagonal only matrix", {
  cov <- matrix(data = c(1,0,0,1), nrow = 2, ncol = 2)

  ans <- simpar:::sblock(n = 3, df = 100, cov = cov)
  expect_true(all(ans[, 2] == 0))
})

test_that("sblock: simulate non-diagonal matrix", {
  cov <- matrix(data = c(1,0.1,0.1,1), nrow = 2, ncol = 2)

  ans <- simpar:::sblock(n = 3, df = 100, cov = cov)
  expect_true(all(ans[, 2] != 0))
})

# omega_diag argument in simpar
test_that("simpar: diagonal omega matrix with omega_diag == TRUE", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- bmat(1, 0, 3)
  sigma <- matrix(1)

  ans <- simpar(n = 5, theta, covar, omega, sigma, 10, 10, omega_diag = TRUE)
  expect_true(all(ans[, 6] == 0))
})

test_that("simpar: diagonal omega matrix with omega_diag == FALSE", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- bmat(1, 0, 3)
  sigma <- matrix(1)

  ans <- simpar(n = 5, theta, covar, omega, sigma, 10, 10)
  expect_true(all(ans[, 6] != 0)) # essentially same behaviour as metrumrg::simpar
})

test_that("simpar: full omega matrix with omega_diag == TRUE", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- bmat(1, 0.1, 3)
  sigma <- matrix(1)
  ans <- simpar(n = 5, theta, covar, omega, sigma, 10, 10, omega_diag = TRUE)
  expect_true(all(ans[, 6] != 0)) # essentially same behaviour as metrumrg::simpar
})

test_that("simpar: full omega matrix with omega_diag == FALSE", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- bmat(1, 0.1, 3)
  sigma <- matrix(1)

  ans <- simpar(n = 5, theta, covar, omega, sigma, 10, 10)
  expect_true(all(ans[, 6] != 0))
})

# sigma_diag argument in simpar

test_that("simpar: diagonal sigma matrix with sigma_diag == TRUE", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- bmat(1, 0, 3)
  sigma <- bmat(1, 0, 3)

  ans <- simpar(n = 5, theta, covar, omega, sigma, 10, 10, sigma_diag = TRUE)
  expect_true(all(ans[, 9] == 0))
})

test_that("simpar: diagonal sigma matrix with sigma_diag == FALSE", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- bmat(1, 0, 3)
  sigma <- bmat(1, 0, 3)

  ans <- simpar(n = 5, theta, covar, omega, sigma, 10, 10)
  expect_true(all(ans[, 9] != 0)) # essentially same behaviour as metrumrg::simpar
})

test_that("simpar: full sigma matrix with sigma_diag == TRUE", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- bmat(1, 0, 3)
  sigma <- bmat(1, 0.1, 3)
  ans <- simpar(n = 5, theta, covar, omega, sigma, 10, 10, sigma_diag = TRUE)
  expect_true(all(ans[, 9] != 0)) # essentially same behaviour as metrumrg::simpar
})

test_that("simpar: full sigma matrix with sigma_diag == FALSE", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- bmat(1, 0, 3)
  sigma <- bmat(1, 0.1, 3)

  ans <- simpar(n = 5, theta, covar, omega, sigma, 10, 10)
  expect_true(all(ans[, 9] != 0))
})

# Check returned error messages

test_that("Error message: dimension not match between theta and covar", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 3, 3)
  omega <- bmat(1, 0, 3)
  sigma <- bmat(1, 0.1, 3)

  expect_error(simpar(n = 5, theta, covar, omega,
                      sigma, 10, 10, sigma_diag = TRUE),
               "order of covar is not equal to length theta")
})

test_that("Error message: non-positive definite covar", {
  theta <- c(1,2,3,4)
  covar <- bmat(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  omega <- bmat(1, 0, 3)
  sigma <- bmat(1, 0.1, 3)

  expect_error(simpar(n = 5, theta, covar, omega,
                      sigma, 10, 10, sigma_diag = TRUE),
               "covar is not positive-definite.")
})

test_that("Error message: omega blocks must be square matrices", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- matrix(data = c(1, 0, 3, 0.1, 0.1, 0.2), nrow = 2)
  sigma <- bmat(1, 0.1, 3)

  expect_error(simpar(n = 5, theta, covar, omega,
                      sigma, 10, 10, sigma_diag = TRUE),
               "not all omega blocks are square")
})

test_that("Error message: sigma blocks must be square matrices", {
  theta <- c(1,2,3,4)
  covar <- diag(0.1, 4, 4)
  omega <- bmat(1, 0, 3)
  sigma <- matrix(data = c(1, 0, 3, 0.1, 0.1, 0.2), nrow = 2)

  expect_error(simpar(n = 5, theta, covar, omega,
                      sigma, 10, 10, sigma_diag = TRUE),
               "not all sigma blocks are square")
})



