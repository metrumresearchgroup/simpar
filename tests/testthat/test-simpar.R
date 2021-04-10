library(simpar)
library(testthat)

context("test-simpar")

bmat <- function (..., context = NULL) {
  x <- list(...)
  x <- as.numeric(x)
  if (length(x) == 1)
    return(matrix(x, nrow = 1, ncol = 1))
  n <- 0.5 * (sqrt(1 - 4 * (-2 * length(x))) - 1)
  if (!n == as.integer(n)) {
    stop(paste0("Block matrix has invalid specification (",
                context, ")."), call. = FALSE)
  }
  mat <- diag(n)
  mat[upper.tri(mat, diag = TRUE)] <- x
  mat <- mat + t(mat) - diag(diag(mat))
  mat
}

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
  expect_equal(round(v2), 2)
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



