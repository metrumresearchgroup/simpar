
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simpar

<!-- badges: start -->
<!-- badges: end -->

The goal of simpar is to …

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
#>        TH.1     TH.2     TH.3     OM1.1    SG1.1
#> 1   0.08210  0.53500 -0.14720    0.4755 258.7000
#> 2  -0.08513  0.20470  0.53180    6.9530   0.3100
#> 3   0.26790  0.30140  0.65140 1085.0000   0.6675
#> 4   0.02566  0.09764  0.90900    0.3965   0.5446
#> 5   0.33830 -0.17910  0.50890    0.4659  72.8300
#> 6  -0.04585  0.14630  0.53550    1.1240   7.6150
#> 7  -0.38350 -0.08626  0.19240  111.2000   0.5025
#> 8   0.31790 -0.47470  0.22190    1.4420   0.7208
#> 9   0.20660 -0.27210  0.02172    0.5200   0.8957
#> 10  0.22950  0.04432  0.46700    1.5770  53.7100
```
