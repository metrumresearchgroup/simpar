
library(microbenchmark)
library(simpar)

# 1) compare non-mrgsolve style versus mrgsolve style
set.seed(1234)

theta <- c(1,2,3,4)
covar <- diag(0.1, 4, 4)
omega <- bmat(1, 0, 3)
sigma <- list(bmat(1, 0.1, 1, 0.1, 0.1, 1), # 3x3
              bmat(1, 0, 3),
              bmat(2, 0.5, 6))

microbenchmark(
  simpar(n = 1000,
         theta, covar,
         omega, sigma,
         10, c(10,10,10),
         omega_diag = TRUE,
         sigma_diag = TRUE),
  simpar(n = 1000,
         theta, covar,
         omega, sigma,
         10, c(10,10,10),
         omega_diag = TRUE,
         sigma_diag = TRUE,
         mrgsolve_style = TRUE)
) %>% print(unit = "relative")

#      min       lq     mean   median       uq      max neval
# 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100
# 1.228985 1.229855 1.218933 1.233948 1.222843 1.171056   100

# 2) compare omega_diag == TRUE vs omega_diag == FALSE
set.seed(1234)

theta <- c(1,2,3,4)
covar <- diag(0.1, 4, 4)
omega <- bmat(1, 0, 3)
sigma <- list(bmat(1, 0.1, 1, 0.1, 0.1, 1), # 3x3
              bmat(1, 0, 3),
              bmat(2, 0.5, 6))

microbenchmark(
  simpar(n = 1000,
         theta, covar,
         omega, sigma,
         10, c(10,10,10),
         omega_diag = TRUE,
         sigma_diag = TRUE),
  simpar(n = 1000,
         theta, covar,
         omega, sigma,
         10, c(10,10,10),
         omega_diag = FALSE,
         sigma_diag = TRUE,
         mrgsolve_style = TRUE)
) %>% print(unit = "relative")

#      min       lq     mean   median       uq      max neval
# 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100
# 1.708044 1.706666 1.672154 1.694742 1.613318 1.528872   100

# 3) complex omega matrix versus simple omega matrix
set.seed(1234)

theta <- c(1,2,3,4)
covar <- diag(0.1, 4, 4)
omega1 <- bmat(1, 0, 3)
omega2 <- list(bmat(2, 0.1, 2, 0.1, 0.1, 2), # 3x3
               bmat(2, 0, 6),
               bmat(4, 0.5, 12))
sigma <- list(bmat(1, 0.1, 1, 0.1, 0.1, 1), # 3x3
              bmat(1, 0, 3),
              bmat(2, 0.5, 6))

microbenchmark(
  simpar(n = 1000,
         theta, covar,
         omega1, sigma,
         10, c(10,10,10),
         omega_diag = FALSE,
         sigma_diag = TRUE),
  simpar(n = 1000,
         theta, covar,
         omega2, sigma,
         c(10,10,10),, c(10,10,10),
         omega_diag = FALSE,
         sigma_diag = TRUE,
         mrgsolve_style = TRUE)
) %>% print(unit = "relative")

#      min       lq     mean   median       uq     max neval
# 1.000000 1.000000 1.000000 1.000000 1.000000 1.00000   100
# 1.922554 1.886074 1.895324 1.903219 1.910783 1.87138   100


