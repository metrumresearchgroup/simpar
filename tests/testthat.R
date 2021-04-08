
Sys.setenv("R_TESTS" = "")
library(simpar)
test_check("simpar", reporter="summary")
