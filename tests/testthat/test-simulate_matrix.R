library(simpar)
library(testthat)

context("test-simulate_matrix")

n <- 5
df <- c(10, 10, 10)
cov <- list(bmat(1, 0.1, 1, 0.1, 0.1, 1),
            bmat(1, 0, 3),
            bmat(2, 0.5, 6))

test_that("omega matrix style matches simpar", {
  simulate_matrix(n, df, cov, style = "simpar")
  simulate_matrix(n, df, cov, style = "mrgsolve")

})


omg <- lapply(seq_along(cov), function(x) list(n = n, df = df[[x]], cov = cov[[x]]))

lapply(omg, function(x) do.call(sblock, x))

simulate_matrix(n, df, cov, style = "simparr")

