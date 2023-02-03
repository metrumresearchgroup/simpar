library(simpar)
library(testthat)

context("test-helper")

test_that("tell square matrix", {
  a <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
  b <- matrix(c(1,0,0.5,0,0.5,1), nrow = 2, ncol = 3)
  expect_true(is.square(a))
  expect_false(is.square(b))
})

test_that("tell diagonal matrix", {
  a <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
  b <- matrix(c(1,0.1,0.5,1), nrow = 2, ncol = 2)
  expect_true(is.diagonal(a))
  expect_false(is.diagonal(b))
})
