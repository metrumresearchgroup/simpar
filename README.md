
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
sigma <- matrix(0.025)
simpar(10, theta, covar, omega, sigma, odf = 100, sdf = 1000)
#>        TH.1      TH.2    TH.3  OM1.1   SG1.1
#> 1  -0.15050  0.002991 -0.1527 1.0920 0.02626
#> 2   0.40290  0.590700  0.2461 1.1160 0.02528
#> 3  -0.01761  0.392800  0.1293 0.9291 0.02563
#> 4   0.06225 -0.051720  0.1767 1.1180 0.02545
#> 5  -0.22530  0.040420  0.5298 1.2600 0.02522
#> 6   0.01525  0.197500  0.3034 0.7389 0.02398
#> 7  -0.09135 -0.074840 -0.1018 1.2370 0.02530
#> 8  -0.05287  0.162100  0.2894 0.8426 0.02548
#> 9   0.06726  0.220900 -0.3004 0.9600 0.02449
#> 10 -0.15660 -0.181500  0.2087 1.0200 0.02473
```
