library(testthat)
library(BoonAmber)

Sys.setenv("AMBER_TEST_LICENSE_ID" = "default")

test_check("BoonAmber")
