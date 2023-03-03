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


test_that("calculate order of a square matrix", {
  a <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
  b <- matrix(c(1,0,0.5,0,0.5,1), nrow = 2, ncol = 3)
  c <- matrix(c(1,0.1,0.5,1,3,5,7,9,9), nrow = 3, ncol = 3)
  expect_true(ord(a) == 2)
  expect_error(ord(b), "matrix is not square")
  expect_false(ord(c) == 2)
})

test_that("calculate half matrix", {
  a <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
  b <- matrix(c(1,0,0.5,0,0.5,1), nrow = 2, ncol = 3)

  expect_is(half(a), "halfmatrix")
  expect_equal(half(a)[[1]], 1)
  expect_equal(half(a)[[2]], 0)
  expect_equal(half(a)[[3]], 1)
  expect_equal(names(half(a))[[1]], "1.1")
  expect_equal(names(half(a))[[2]], "2.1")
  expect_equal(names(half(a))[[3]], "2.2")

  expect_error(half(b), "matrix is not symmetric")

})

test_that("extract off diagonal elements of a a half matrix", {
  a <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
  a <- half(a)
  expect_true(offdiag(a) == 0)

  b <- matrix(c(1,0.1,0.1,0.1,1,0.1,0.1,0.1,1), nrow=3, ncol=3)
  b <- half(b)
  expect_true(all(offdiag(b) == c(0.1,0.1,0.1)))
})


test_that("calculate order of half matrix", {
  a <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
  a <- half(a)
  expect_true(ord.halfmatrix(a) == 2)
})

test_that("make matrix based on halfmatrix", {
  a <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
  half <- half(a)
  expect_equal(as.matrix.halfmatrix(half), a)
  expect_is(as.matrix.halfmatrix(half), "matrix")
})








