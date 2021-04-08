
Sys.setenv("R_TESTS" = "")
library(simpar)
library(testthat)
test_check("simpar", reporter="summary")
