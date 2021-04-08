
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simpar

<!-- badges: start -->
<!-- badges: end -->

The goal of simpar is to create parameters for simulation with
uncertainty.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("metrumresearchgroup/simpar")
```

## Example

This is a basic example which shows you how to do a basic simulation:

``` r
library(simpar)
theta <- c(1,2,3)/10
covar <- diag(0.1, 3)
omega <- matrix(1)
sigma <- matrix(1)
simpar(10, theta, covar, omega, sigma)
#>         TH.1     TH.2     TH.3    OM1.1    SG1.1
#> 1  -0.013720  0.99360  0.53840   0.7465   0.4274
#> 2  -0.001642  0.33570  1.01900   3.3230   1.6860
#> 3  -0.049110  0.34660  0.08864   2.9760   1.1950
#> 4  -0.287900  0.01976  0.28350   0.2194   5.1310
#> 5   0.320500 -0.12600  0.77330  32.9000   5.2760
#> 6   0.004636  0.18730  0.17920 111.7000   1.0290
#> 7  -0.092990  0.48920  0.32050   1.0290   0.4270
#> 8  -0.117800  0.55460 -0.34780   0.5605   2.4890
#> 9   0.030300  0.33980  0.50110  64.1000   9.9470
#> 10 -0.405300  0.40580  0.24340   0.6069 185.9000
```
