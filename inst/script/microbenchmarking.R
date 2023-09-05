
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

mc1 <- microbenchmark(
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
)

mc1
# Unit: seconds
#      min       lq     mean   median       uq      max neval
# 1.180873 1.227210 1.298364 1.265389 1.353498 1.699752   100
# 1.453516 1.519692 1.616580 1.573685 1.684875 2.038829   100

mc1 %>% print(unit = "relative")
# Unit: relative
#      min      lq    mean   median      uq      max neval
# 1.000000 1.00000 1.00000 1.000000 1.00000 1.000000   100
# 1.230883 1.23833 1.24509 1.243637 1.24483 1.199486   100

# 2) compare omega_diag == TRUE vs omega_diag == FALSE
set.seed(1234)

theta <- c(1,2,3,4)
covar <- diag(0.1, 4, 4)
omega <- bmat(1, 0, 3)
sigma <- list(bmat(1, 0.1, 1, 0.1, 0.1, 1), # 3x3
              bmat(1, 0, 3),
              bmat(2, 0.5, 6))

mc2 <- microbenchmark(
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
         sigma_diag = TRUE)
)

mc2
# Unit: seconds
#      min       lq     mean   median       uq      max neval
# 1.187788 1.222746 1.311994 1.288521 1.371487 1.682886   100
# 1.756218 1.790779 1.911751 1.869888 1.987210 2.538151   100

mc2 %>% print(unit = "relative")
# Unit: relative
# min       lq     mean   median       uq      max neval
# 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100
# 1.478561 1.464555 1.457134 1.451189 1.448946 1.508213   100

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

mc3 <- microbenchmark(
  simpar(n = 1000,
         theta, covar,
         omega1, sigma,
         10, c(10,10,10),
         omega_diag = FALSE,
         sigma_diag = TRUE),
  simpar(n = 1000,
         theta, covar,
         omega2, sigma,
         c(10,10,10), c(10,10,10),
         omega_diag = FALSE,
         sigma_diag = TRUE)
)

mc3
# Unit: seconds
#      min       lq     mean   median       uq      max neval
# 1.754600 1.791938 1.921014 1.888674 1.988851 2.449862   100
# 2.966021 3.005774 3.209475 3.122042 3.355769 3.994624   100

mc3 %>% print(unit = "relative")
# Unit: relative
#      min       lq     mean   median       uq      max neval
# 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100
# 1.690426 1.677387 1.670719 1.653034 1.687291 1.630551   100


